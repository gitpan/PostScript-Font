# BasicTypesetter.pm --  Module for basic PostScript typesetting
# RCS Info        : $Id: BasicTypesetter.pm,v 1.6 2000-06-23 09:09:44+02 jv Exp $
# Author          : Johan Vromans
# Created On      : Sun Jun 18 11:40:12 2000
# Last Modified By: Johan Vromans
# Last Modified On: Fri Jun 23 09:09:20 2000
# Update Count    : 455
# Status          : Unknown, Use with caution!

package PostScript::BasicTypesetter;

$VERSION = 1.0;

use 5.005;
use strict;
use PostScript::FontMetrics;
use PostScript::ISOLatin1Encoding;
use Carp;
use constant FONTSCALE => 1000;

my $trace;
my $verbose;
my $debug;
my $error;

sub _error {
    if ( $error eq 'die' ) {
	croak (@_);
    }
    else {
	carp (@_);
    }
}

=head1 NAME

PostScript::BasicTypesetter - Module for basic typesetting

=head1 SYNOPSIS

    # Create an object directly from the AFM file.
    my $ts = new PostScript::BasicTypesetter("/usr/share/metrics/times.afm");

    # Alternatively, use the Unix PostScript Resources.
    my $psres = new PostScript::Resources;
    my $m = new PostScript::FontMetrics($psres->FontAFM("Times-Roman"));
    # The metrics object can now be shared between typesetters.
    my $ts = new PostScript::BasicTypesetter($m);

    # Re-encode the font to use ISO Latin-1 encoding.
    # The resultant font will be named Times-Romand-Latin1.
    $ts->reencode("ISOLatin1Encoding", "Latin1");

    # Set the current font size to 10 points.
    $ts->fontsize(10);
    # Set the lineskip to 12 points.
    $ts->lineskip(12);

    # Add to the PostScript preamble:
    print $ts->ps_reencodesub;
    print $ts->ps_preamble;

    # Add to the PostScript Setup:
    print $ts->ps_reencode;

    # To typeset a box of text at $x, $y, width $w:
    print $ts->ps_textbox($x,$y,$w,$str);


=head1 DESCRIPTION

PostScript::BasicTypesetter is an experimental module to facilitate
PostScript based typesetting. Its principal reason for existance is
that it properly uses font metrics for precise typesetting and
kerning.

A BasicTypesetter object maintains font information (name, encoding,
and size) that can be used to typeset texts. Every operation invoked
on a typesetter object is executed w.r.t. the current values for the
font, encoding and size. However, once the PostScript preamble has
been constructed, the font name and encoding should not be changed
anymore.

=head1 CONSTRUCTOR

=head2 new

Example:

    $ts = new PostScript::BasicTypesetter ($arg);

This constructs a new typesetter object.

The argument may be a PostScript::FontMetrics object, or the name of a
valid font metrics file.

If the argument refers to a FontMetrics object, this object will be
associated with the Typesetter object, and it can be shared between
Typesetters. If it is a file name, a private FontMetrics object will
be created.

The new typesetter gets a default value of C<10> for the font size,
and C<12> for the lineskip.

=cut

my $def_fontsize = 10;
my $def_lineskip = 12;

sub new {
    my $class = shift;
    my $metrics = shift;

    my (%atts) = (error => 'die',
		  verbose => 0, trace => 0,
		  @_);
    $debug = lc($atts{debug});
    $trace = $debug || lc($atts{trace});
    $verbose = $trace || lc($atts{verbose});
    $error = lc($atts{error});
    my $self = {};

    # Need either a PostScript::FontMetrics object, or a file name.
    if ( UNIVERSAL::isa($metrics, 'PostScript::FontMetrics') ) {
	$self->{metrics} = $metrics;
    }
    else {
	$self->{metrics} = new PostScript::FontMetrics ($metrics, %atts);
    }
    bless $self, $class;
    $self->{fontsize} = $def_fontsize;
    $self->{lineskip} = $def_lineskip;
    $self;
}

=head2 clone

Example:

    $newts = $ts->clone;	# identical
    $ts12 = $ts->clone(12);	# identical, but font size 12

Creates a new typesetter with identical contents and shared metrics.
If additional arguments are present, these are passed to the
C<fontsize> method.

=cut

sub clone {
    my $self = shift;
    $self = bless { %$self }, ref($self);
    $self->fontsize(@_) if @_;
    $self;
}

=head1 INSTANCE METHODS

=head2 fontname

Example:

    $name = $ts->fontname;

This routine returns the name of the font, which may differ from the
real font name if the font has been reencoded. For example,
re-encoding Times-Roman (the real font name) may result in a font that
will be known as Times-Roman-Latin1. 

=cut

sub fontname {
    my ($self) = @_;

    # If the font has been reencoded, return the new font name.
    $self->metrics->{tp_encodedfontname} || $self->metrics->FontName;
}

=head2 real_fontname

Example:

    $name = $ts->real_fontname;

This routine returns the real name of the font.

=cut

sub real_fontname {
    my ($self) = @_;

    $self->metrics->FontName;
}

=head2 metrics

    $metrics = $ts->metrics;

This routine returns provides access to the associated
PostScript::FontMetrics object.

=cut

sub metrics {
    my ($self) = @_;

    $self->{metrics};
}

=head2 reencode

Example:

    $ts->reeencode ($enc, $tag, $changes);

This routine re-encodes the font associated with this typesetter
according to the specified encoding, and optionally modifies the
resultant encoding according to the changes vector. The new font will
get the name of the original font, with C<->I<tag> appended.

C<$baseenc> must be either C<"StandardEncoding"> (default) or
C<"ISOLatin1Encoding">.

C<$changes>, if specified, must be a reference to a hash. For each key
of the hash, its ordinal value in the resultant encoding will be set
to its value, which must be a valid glyph name.

For example, to re-encode a font to ISO-Latin1 and add the glyph
C<ellipsis> to location 0200 (octal), the following call can be used:

    $ts->reencode ("ISOLatin1Encoding", "Latin1", {"\200" => "ellipsis"});

B<WARNING:> C<reencode> affects the FontMetrics object that is
associated with this Typesetter. When FontMetrics objects are shared
between Typesetters, Calling C<reencode> on one of the Typesetters will
affect the other Typesetters as well. This is deliberate.

=cut

# Reencode a font to a specific base, with small modifications.
sub reencode {
    my ($self, $base, $tag, $vec) = @_;

    my $baseenc = $base || "StandardEncoding";

    # Check encoding, and get a ref to the array.
    if ( $baseenc eq "StandardEncoding" ) {
	require PostScript::StandardEncoding;
	$base = PostScript::StandardEncoding->array;
    }
    elsif ( $baseenc eq "ISOLatin1Encoding" ) {
	$base = PostScript::ISOLatin1Encoding->array;
    }
    else {
	_error ("Invalid encoding: $baseenc");
	return;
    }

    # Reencode according to the vector..
    if ( $vec ) {
	$base = [@$base];		# copy
	my ($k, $v);
	while ( ($k,$v) = each (%$vec) ) {
	    $base->[ord($k)] = $v;
	}
	$self->metrics->{tp_reencodevector} = { %$vec };
    }
    else {
	undef $self->metrics->{tp_reencodevector};
    }

    # Set the new encoding.
    $self->metrics->setEncoding ($base);

    # Form a new name for the font.
    $self->metrics->{tp_encodedfontname} =
      $self->{metrics}->FontName . "-" . $tag;

}

=head2 fontsize

Example:

    $ts->fontsize(12, 15);
    $size = $ts->fontsize;

Sets or gets the current font size. When setting, the (optional)
second argument can be used to simultaneously set the lineskip.

=cut

sub fontsize {
    my ($self, $size, $skip) = @_;
    $self->{fontsize} = $size if $size;
    $self->{lineskip} = $skip if $skip;
    $self->{fontsize};
}

=head2 lineskip

Example:

    $ts->lineskip(15);
    $skip = $ts->lineskip;

Sets or gets the current lineskip.

=cut

sub lineskip {
    my ($self, $skip) = @_;
    $self->{lineskip} = $skip if $skip;
    $self->{lineskip};
}

=head2 stringwidth

Example:

    $width = $ts->stringwidth($str);

Returns the width of the string for the current font and size,
with kerning information applied.

=cut

sub stringwidth {
    my $self = shift;

    my ($str) = @_;
    my $size = $self->{fontsize};
    $self->metrics->kstringwidth($str)*$size/FONTSCALE;
}

# Internal helper w/o kerning.

sub _stringwidth {
    my $self = shift;

    my ($str) = @_;
    my $size = $self->{fontsize};
    $self->metrics->stringwidth($str)*$size/FONTSCALE;
}

=head2 tjvector

Example:

    $vec = $ts->tjvector($str);

Returns the typesetting vector for the string, with kerning
information applied. This vector can be passed to the C<ps_tj>
method.

=cut

sub tjvector {
    my $self = shift;

    $self->metrics->kstring(@_);
}

################ PostScript code builders ################

# All ps_ routines return a printable PostScript string.

=head2 ps_preamble

Example:

    print $ts->ps_preamble;

Produces the PostScript code for the preamble.

To be used while constructing the PostScript preamble.

=cut

sub ps_preamble {
    my $self = shift;
    <<EOD;
% TJ operator to print typesetinfo vectors.
% Requires Fpt to be defined!
/TJ {
  { dup type /stringtype eq { show } { Fpt mul 0 rmoveto } ifelse }
  forall
} bind def
EOD
}

=head2 ps_reencodesub

Example:

    print $ts->ps_reencodesub (name => "ReEncode");

Produces the PostScript code to define a PostScript routine to reencode
the fonts.

To be used while constructing the PostScript preamble.

Attributes:

=over

=item name

The name prefix of the subroutine. If the reencoding vector is
embedded (see below), this is the actual name for the routine.
Otherwise, the actual name for the routine is I<prefix>C<Sub>, and the
vector will be named I<prefix>C<Vec>.

=item base

The base encoding. This should be either C<StandardEncoding> (default)
or C<ISOLatin1Encoding>.

=item vec

If this is C<"embedded"> (default), the reencoding vector will be part
of the subroutine. Otherwise it will be defined separately.

=back

=cut

sub ps_reencodesub {
    my ($self) = shift;

    my %atts = ( name => "ReEncode",
		 base => "AdobeStandardEncoding",
		 vec  => "embedded",
		 @_);

    my $base = $atts{base};
    my $name = $atts{name};

    my $ret = "";

    if ( $atts{vec} eq "embedded" ) {
	$ret .= "/${name} {\n  /newcodesandnames [\n";
	my ($k, $v);
	while ( ($k,$v) = each (%{$self->metrics->{tp_reencodevector}}) ) {
	    $ret .= sprintf ("    8#%03o /%s\n", ord($k), $v);
	}
	$ret .= "  ] def\n";
    }
    else {
	$ret .= "/${name}Sub {\n  /newcodesandnames exch def\n";
    }
    $ret .= <<EOD;
  /newfontname exch def
  /basefontname exch def

  /basefontdict basefontname findfont def
  /newfont basefontdict maxlength dict def
  basefontdict
  { exch dup /FID ne
    { dup /Encoding eq
      { exch pop $base dup length array copy
        newfont 3 1 roll put
      }
      { exch newfont 3 1 roll put }
      ifelse
    }
    { pop pop }
    ifelse
  }
  forall

  newfont /FontName newfontname put
  newcodesandnames aload pop
  newcodesandnames length 2 idiv
  { newfont /Encoding get 3 1 roll put }
  repeat

  newfontname newfont definefont pop
} def
EOD

    if ( $atts{vec} ne "embedded" ) {
	$ret .= "/${name}Vec [\n";
	my ($k, $v);
	while ( ($k,$v) = each (%{$self->metrics->{tp_reencodevector}}) ) {
	    $ret .= sprintf ("  8#%03o /%s\n", ord($k), $v);
	}
	$ret .= "] def\n";
    }

    $ret;
}

=head2 ps_reencode

Example:

    print $ts->ps_reencode (name => "ReEncode");

Produces the PostScript code to reencode a font.
Nothing is produced if the font has not been re-encoded.

To be used while constructiong the PostScript C<Setup> section.

Attributes:

=over

=item name

The name prefix of the subroutine. If the reencoding vector is
embedded (see below), this is the actual name for the routine.
Otherwise, the actual name for the routine is I<prefix>C<Sub>, and the
vector will be named I<prefix>C<Vec>.

=item vec

If this is C<"embedded"> (default), the reencoding vector will be
assumed part of the subroutine. Otherwise it will be specified
separately.

=back

=cut

sub ps_reencode {
    my $self = shift;
    my %atts = ( name => "ReEncode",
		 vec => "embedded",
		 @_);

    return "" if $self->{metrics}->FontName eq $self->fontname;

    my $name = $atts{name};
    "/" . $self->{metrics}->FontName . " " .
      "/" . $self->fontname . " " .
	($atts{vec} eq "embedded" ? "$name" :
	 ($name . "Vec " . $name . "Sub)")) . "\n";
}


=head2 ps_str

Example:

    print $ts->ps_str ($str);

Produces the PostScript representation for the given string.

=cut

sub ps_str {
    my $self = shift;

    # Externalize a PostScript string.
    local ($_) = @_;
    s/([\\\200-\377()])/sprintf("\\%03o",ord($1))/eg;
    '(' . $_ . ')';
}

=head2 ps_setfont

Example:

    print $ts->ps_setfont;

Produces the PostScript code to designate the current font and size
for PostScript.

This method keeps track of the settings, and will not produce
anything if the current settings are already as requested. Hence use
liberally.

=cut

my $ps_curfont;

sub ps_setfont {
    my $self = shift;

    my $ret = '';
    my $size = $self->{fontsize};
    croak ("ps_setfont: Font size not set") unless $size;
    unless ( $ps_curfont && $ps_curfont eq "$size $self->fontname" ) {
	$ret .= sprintf ("/%s findfont %.3g scalefont setfont\n",
			 $self->fontname, $size);
	$ps_curfont = "$size $self->fontname";
    }
    $ret;
}

=head2 ps_tj

Example:

    print $ts->ps_tj ($tj);

Produces the PostScript code to print the text at the current position.
The argument to this function must be the result of a call to C<tjvector>.

=cut

my $ps_curFpt;

# Print a typesetting vector. Use TJ definition.
sub ps_tj {
    my $self = shift;
    local ($_);

    my ($t) = @_;
    my $ret = '';
    my $size = $self->{fontsize};
    croak ("ps_tj: Font size not set") unless $size;
    unless ( $ps_curFpt && $ps_curFpt eq $size/FONTSCALE ) {
	$ps_curFpt = $size/FONTSCALE;
	$ret .= "/Fpt $ps_curFpt def\n";
    }
    $ret .= "[";
    my $l = 1;
    foreach ( @$t ) {
	$_ = sprintf("%g", $_) unless /^\(/;
	if ( ($l += length) >= 80 ) {
	    $ret .= "\n ";
	    $l = 1 + length;
	}
	$ret .= $_;
    }
    $ret .= "] TJ\n";
    $ret;
}

=head2 ps_textbox

Example:

    print $ts->ps_textbox ($x, $xi, $y, $w, $str, $align);

Produces the PostScript code to typeset a string, or a set of strings.

The string will be printed starting at base postion C<$x> and C<$y>.
If the string exceeds the width C<$w> a line wrap will occur.
The first line will be indented with C<$xi>.

If C<$str> is a reference to an array, this array may contain a mix of
strings, PostScript::BasicTypesetter objects, and array references
(recursive). Each string is typeset according to the current
typesetter; a typesetter object changes the current typesetter for the
rest of the array. However, the lineskip value of the initial
typesetter is used for linewraps, regardless the typesetter currently
in control.

C<$xi> and/or C<$y> may be references to the actual values. In this
case, the corresponding values will be updated to reflect the value
upon completion. In other words, the resulting C<$xi> value will
reflect the x-position directly after the last character of the last
line. The C<$y> value will reflect the baseline of the last line that
was typeset. Using references to the actual values, series of calls to
C<ps_textbox> can be chained: each call will continue exactly where
the preceding call left off. This is, of course, only useful with
flush left alignment.

Values for C<$align> are C<"l"> (default): flush left, C<"r">: flush
right, C<"c">: centered, C<"j">: justified.

=cut

sub ps_textbox {
    my ($self, $x, $xxi, $yy, $width, $t, $align) = @_;

    # Default is flush left.
    $align = (defined $align) ? lc($align) : 'l';
    croak ("ps_textbox: Unhandled alignment '$align'")
      unless $align =~ /^[lrjc]$/;

    return _ps_simpletextbox  ($self, $x, $xxi, $yy, $width, $t, $align)
      unless ref ($t);

    return _ps_textbox  ($self, $x, $xxi, $yy, $width, $t, $align);
}

# Internal helper routine. This is for the multi-font case.
sub _ps_textbox {
    my ($self, $x, $xxi, $yy, $width, $t, $align) = @_;

    # Default is flush left.
    $align = (defined $align) ? lc($align) : 'l';
    croak ("ps_textbox: Unhandled alignment '$align'")
      unless $align =~ /^[lrjc]$/;

    my $cur = $self;
    my $cur0 = $cur;

    # Deref arguments, if needed.
    my $xi = ref($xxi) ? $$xxi : $xxi;
    my $y = ref($yy) ? $$yy : $yy;

    my $scale;			# 1000/fontsize
    my $wspace;			# width of a space
    my $wd = $xi;		# accumulated width
    my $ret = '';		# accumulated output
    my $lskip = $cur0->lineskip; # line skip (fixed)

    # Setup global values for this font.
    my $switch_font = sub {
	$scale = FONTSCALE/$cur->{fontsize};
	$wspace = $cur->_stringwidth(" ");
	croak ("ps_textbox: [".$cur->metrics->FontName."]: missing space")
	  unless $wspace > 0;
    };

    my @res;
    my $overflow;		# we did a line wrap

    # This is the actual typesetting routine.
    my $flush = sub {
	$cur = $cur0;
	$switch_font->();

	$wd = $xi;
	my $t = [$cur];		# current ts vector
	my $tt = [];		# accumulated vectors
	my $nsp = 0;		# available space for stretching

	# Discard possible leading/trailing space.
	shift(@res) if $overflow && !ref($res[0]);
	pop (@res) unless ref($res[-1]);

	foreach ( @res ) {
	    if ( ref($_) ) {
		if ( UNIVERSAL::isa($_,PostScript::BasicTypesetter::) ) {
		    push (@$tt, $t) if @$t;
		    $t = [$cur = $_];
		    $switch_font->();
		    next;
		}
		# width/kstring pair -> collect the string and tally the width.
		$wd += shift(@$_);
		push (@$t, $_);	# push as ref
	    }
	    else {
		# Width. Collect (scaled).
		push (@$t, $_*$scale);
		$wd += $_;
		# Tally available space.
		$nsp += $_ if $align eq "j";
	    }
	}
	# Collect final ts vector.
	push (@$tt, $t) if @$t;

	# Calculate initial position, and set it.
	$xi = $width - $wd if $align eq "r";
	$xi = ($width - $wd)/2 if $align eq "c";
	$ret .= sprintf ("%.2f %.2f moveto\n", $x+$xi, $y);

	# Calculate amount of stretch needed.
	my $stretch = 1;
	$stretch = ($width - $wd + $nsp) / $nsp
	  if $align eq "j" && $nsp;

	# Process the vectors.
	foreach $t ( @$tt ) {
	    $cur = shift(@$t);
	    $ret .= $cur->ps_setfont;
	    $t = [map { ref($_) ? @$_ : $_*$stretch } @$t];
	    $ret .= $cur->ps_tj ($t);
	}
    };

    # Subroutine code starts here.

    # Preprocessing of the arguments. Keep everything, but split the
    # strings.
    my @args;
    my $preparse;
    $preparse = sub {
	my $t = shift;
	foreach ( @$t ) {
	    if ( ref($_) ) {
		if ( UNIVERSAL::isa($_,PostScript::BasicTypesetter::) ) {
		    push (@args, $cur = $_);
		    next;
		}
		# Array ref introduces new context.
		# Tack current font at end.
		push (@$_, $cur);
		$preparse->($_);
		next;
	    }
	    s/\s+/ /g if $align eq "j";
	    push (@args, split (/( )/, $_));
	}
    };
    $t = [$t] unless ref($t) && ref($t) eq "ARRAY";
    $preparse->($t);

    # @args is now a list of strings (some of which are a single space),
    # and typesetter objects.
    # print STDERR Data::Dumper->Dump([\@args],[qw{args}]);

    $cur = $cur0;
    $switch_font->();
    foreach my $arg ( @args ) {

	# Skip empty args resulting from strings that start with a space.
	next if $arg eq '';

	# Typesetter object.
	if ( ref($arg) ) {
	    push (@res, $cur = $arg);
	    $switch_font->();
	    next;
	}

	# Width of this thing.
	my $w = $arg eq " " ? $wspace : $cur->stringwidth($arg);

	# See if it fits.
	if ( $wd + $w > $width ) {

	    # No. Fill what we have.
	    $flush->();

	    # Advance to next line.
	    $y -= $lskip;

	    # Reset.
	    @res = ();
	    $wd = 0;
	    $xi = 0;
	    $cur0 = $cur;
	    $overflow++;
	}
	# It fits -> append.
	# Push strings as a [width,kstring] pair, and spaces as a
	# number.
	push (@res, $arg eq " " ? $w : [$w, $cur->metrics->kstring($arg)]);
	$wd += $w;
    }

    # Process remainder.
    if ( @res ) {
	$align = "l" if $align eq "j"; # disable justify
	$flush->();
	# Update indent value.
	$xi = $wd;
    }
    elsif ( ref($yy) ) {
	$y += $lskip;	# already updated, so fix it
    }

    # Update return values.
    $$yy = $y if ref($yy);
    $$xxi = $xi if ref($xxi);
    $ret;
}

# Internal helper routine. This is for the single-font case.
# About 30% faster.
sub _ps_simpletextbox {
    my ($self, $x, $xxi, $yy, $width, $t, $align) = @_;

    # Deref arguments, if needed.
    my $xi = ref($xxi) ? $$xxi : $xxi;
    my $y = ref($yy) ? $$yy : $yy;

    # Accumulated output.
    my $ret = $self->ps_setfont();

    # Scaling for fill, only when justifying.
    my $scale = ($align eq 'j') ? FONTSCALE/$self->fontsize : 0;

    # Width of a space.
    my $wspace = $self->_stringwidth(" ");
    croak ("ps_textbox: [".$self->real_fontname."]: missing space")
      unless $wspace > 0;

    # Accumulated width.
    my $wd = $xi + -$wspace;

    # Line skip (baselines).
    my $lineskip = $self->{lineskip};

    my @res;

    my $flush = sub {
	my $ext = 0;
	$ext = $scale*(($width - $wd) / (@res-1))
	  if $align eq "j" && $scale && @res > 1;
	my $t = $self->metrics->kstring ("@res", $ext);

	# Calculate initial position, and set it.
	$xi = $width - $wd if $align eq "r";
	$xi = ($width - $wd)/2 if $align eq "c";
	$ret .= sprintf ("%.2f %.2f moveto\n", $x+$xi, $y);
	$ret .= $self->ps_tj ($t);
    };

    # Subroutine code starts here.

    # Split into space-separated pieces (let's call them "words").
    my @text = split (/\s+/, $t, -1);
    foreach my $str ( @text ) {
	# Width of this "word".
	my $w = $self->stringwidth($str);
	# See if it fits.
	if ( $wd + $wspace + $w > $width ) {
	    # No -> flush what we have.
	    $flush->();
	    # Advance to next line.
	    $y -= $lineskip;
	    # Reset.
	    @res = ();
	    $wd = -$wspace;
	    $xi = 0;
	}
	# It fits -> append.
	$wd += $wspace + $w;
	push (@res, $str);
    }
    # Process remainder.
    if ( @res ) {
	$align = "l"if $align eq "j";
	$flush->();
	# Update indent value.
	$xi = $wd;
    }
    elsif ( ref($yy) ) {
	$y += $lineskip;	# already updated, so fix it
    }

    # Update return values.
    $$yy = $y if ref($yy);
    $$xxi = $xi if ref($xxi);
    $ret;
}

1;

__END__

=head1 EXAMPLE

    use PostScript::BasicTypesetter;

    my $tr = new PostScript::BasicTypesetter
      ("/usr/lib/ghostscript/fonts/Times-Roman");

    print STDOUT ("%!PS-Adobe-3.0\n",
		  "%%DocumentResources: font ",
		  $tr->metrics->FontName, " ",
		  "%%Pages: 1\n",
		  $tr->ps_preamble,
		  "%%EndPrologue\n",
		  "%%Page 1 1\n");

    $tr->fontsize(150, 200);
    print STDOUT ($tr->ps_textbox (mm(10), 0, mm(200), mm(180),
				   "Perl Rules!", "c"));

    print STDOUT ("showpage\n",
		  "%%Trailer\n",
		  "%%EOF\n");

    # Convert millimeters to PostScript units.
    sub mm { ($_[0] * 720) / 254 }

=head1 AUTHOR

Johan Vromans, Squirrel Consultancy <jvromans@squirrel.nl>

=head1 COPYRIGHT and DISCLAIMER

This program is Copyright 2000 by Squirrel Consultancy. All
rights reserved.

This program is free software; you can redistribute it and/or modify
it under the terms of either: a) the GNU General Public License as
published by the Free Software Foundation; either version 1, or (at
your option) any later version, or b) the "Artistic License" which
comes with Perl.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See either the
GNU General Public License or the Artistic License for more details.

=cut
