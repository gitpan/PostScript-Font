# RCS Status      : $Id: Font.pm,v 1.9 1999-09-24 15:19:11+02 jv Exp $
# Author          : Johan Vromans
# Created On      : December 1999
# Last Modified By: Johan Vromans
# Last Modified On: Fri Sep 24 15:15:51 1999
# Update Count    : 351
# Status          : Looks okay

################ Module Preamble ################

package PostScript::Font;

use strict;

BEGIN { require 5.005; }

use IO;
use File::Spec;

use vars qw($VERSION);
$VERSION = "1.00_01";

# If you have the t1disasm program, have $t1disasm point to it.
# This speeds up the glyph fetching.
# The ttftot42 is used to convert True Type fonts to Type 42.
use vars qw($t1disasm $ttftot42);

# Adobe StandardEncoding.
my @StandardEncoding;
my $StandardEncoding =
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  "space exclam quotedbl numbersign dollar percent ampersand quoteright ".
  "parenleft parenright asterisk plus comma hyphen period slash zero ".
  "one two three four five six seven eight nine colon semicolon less ".
  "equal greater question at A B C D E F G H I J K L M N O P Q R S T U ".
  "V W X Y Z bracketleft backslash bracketright asciicircum underscore ".
  "quoteleft a b c d e f g h i j k l m n o p q r s t u v w x y z ".
  "braceleft bar braceright asciitilde .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef exclamdown cent ".
  "sterling fraction yen florin section currency quotesingle ".
  "quotedblleft guillemotleft guilsinglleft guilsinglright fi fl ".
  ".notdef endash dagger daggerdbl periodcentered .notdef paragraph ".
  "bullet quotesinglbase quotedblbase quotedblright guillemotright ".
  "ellipsis perthousand .notdef questiondown .notdef grave acute ".
  "circumflex tilde macron breve dotaccent dieresis .notdef ring ".
  "cedilla .notdef hungarumlaut ogonek caron emdash .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef AE .notdef ".
  "ordfeminine .notdef .notdef .notdef .notdef Lslash Oslash OE ".
  "ordmasculine .notdef .notdef .notdef .notdef .notdef ae .notdef ".
  ".notdef .notdef dotlessi .notdef .notdef lslash oslash oe germandbls ".
  ".notdef .notdef .notdef .notdef";

# Adobe ISOLatin1Encoding.
my @ISOLatin1Encoding;
my $ISOLatin1Encoding =
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  "space exclam quotedbl numbersign dollar percent ampersand quoteright ".
  "parenleft parenright asterisk plus comma minus period slash zero one ".
  "two three four five six seven eight nine colon semicolon less equal ".
  "greater question at A B C D E F G H I J K L M N O P Q R S T U V W X ".
  "Y Z bracketleft backslash bracketright asciicircum underscore ".
  "quoteleft a b c d e f g h i j k l m n o p q r s t u v w x y z ".
  "braceleft bar braceright asciitilde .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef .notdef .notdef .notdef ".
  ".notdef .notdef .notdef .notdef .notdef dotlessi grave acute ".
  "circumflex tilde macron breve dotaccent dieresis .notdef ring ".
  "cedilla .notdef hungarumlaut ogonek caron space exclamdown cent ".
  "sterling currency yen brokenbar section dieresis copyright ".
  "ordfeminine guillemotleft logicalnot hyphen registered macron degree ".
  "plusminus twosuperior threesuperior acute mu paragraph ".
  "periodcentered cedilla onesuperior ordmasculine guillemotright ".
  "onequarter onehalf threequarters questiondown Agrave Aacute ".
  "Acircumflex Atilde Adieresis Aring AE Ccedilla Egrave Eacute ".
  "Ecircumflex Edieresis Igrave Iacute Icircumflex Idieresis Eth Ntilde ".
  "Ograve Oacute Ocircumflex Otilde Odieresis multiply Oslash Ugrave ".
  "Uacute Ucircumflex Udieresis Yacute Thorn germandbls agrave aacute ".
  "acircumflex atilde adieresis aring ae ccedilla egrave eacute ".
  "ecircumflex edieresis igrave iacute icircumflex idieresis eth ntilde ".
  "ograve oacute ocircumflex otilde odieresis divide oslash ugrave ".
  "uacute ucircumflex udieresis yacute thorn ydieresis";

my $trace;
my $verbose;
my $error;

sub new {
    my $class = shift;
    my $font = shift;
    my (%atts) = (error => 'die',
		  format => 'ascii',
		  verbose => 0, trace => 0,
		  @_);
    my $self = { file => $font };
    bless $self, $class;

    return $self unless defined $font;

    $trace = lc($atts{trace});
    $verbose = $trace || lc($atts{verbose});
    $error = lc($atts{error});
    $atts{format} = "ascii" if lc($atts{format}) eq "pfa";
    $atts{format} = "binary" if lc($atts{format}) eq "pfb";

    $ttftot42 = _getexec ("ttftot42")
      unless defined $ttftot42;
    $t1disasm = _getexec ("t1disasm")
      unless defined $t1disasm;

    eval {

	$self->_loadfont ();

	# Reformat if needed.
	$self->{format} = "ascii";
	if ( lc($atts{format}) eq "asm" ) {
	    print STDERR ($self->{file}, ": Converting to ASM format\n")
	      if $verbose;
	    $self->{data} = $self->_pfa2asm;
	    $self->{format} = "asm";
	}
	elsif ( lc($atts{format}) eq "binary" ) {
	    print STDERR ($self->{file}, ": Converting to Binary format\n")
	      if $verbose;
	    $self->{data} = $self->_pfa2pfb;
	    $self->{format} = "binary";
	}

    };

    if ( $@ ) {
	die ($@)  unless $error eq "warn";
	warn ($@) unless $error eq "ignore";
	return undef;
    }

    $self;
}

sub FileName	{ $_[0]->{file};      }
sub FontName	{ $_[0]->{name};      }
sub FontData	{ ${$_[0]->{data}};   }
sub FamilyName	{ $_[0]->{family};    }
sub FontType	{ $_[0]->{type};      }
sub Version	{ $_[0]->{version};   }
sub ItalicAngle	{ $_[0]->{italic};    }
sub isFixedPitch{ $_[0]->{fixed};     }
sub Weight	{ $_[0]->{weight};    }
sub FontMatrix	{ $_[0]->{fontmatrix};}
sub DataFormat  { $_[0]->{format};    }

sub FontGlyphs {
    my $self = shift;
    $self->{glyphs} = $self->_getglyphnames unless exists $self->{glyphs};
    $self->{glyphs};
}

sub Encoding {
    my $self = shift;
    $self->{encoding} = $self->_getencoding unless exists $self->{encoding};
    $self->{encoding};
}

sub EncodingVector {
    my $self = shift;
    my $enc = $self->{encoding};
    return $enc if ref($enc) eq "ARRAY";
    # Return private copy for the standard encodings.
    if ( $enc eq "StandardEncoding" ) {
	return StandardEncoding();
    }
    elsif ( $enc eq "ISOLatin1Encoding" ) {
	return ISOLatin1Encoding();
    }
    undef;
}

sub StandardEncoding {
    @StandardEncoding = split(' ', $StandardEncoding)
      unless defined @StandardEncoding;
    \@StandardEncoding;
}

sub ISOLatin1Encoding {
    @ISOLatin1Encoding = split(' ', $ISOLatin1Encoding)
      unless defined @ISOLatin1Encoding;
    \@ISOLatin1Encoding;
}

sub _loadfont ($) {

    my $self = shift;
    my $data;			# font data
    my $type;

    my $fn = $self->{file};
    my $fh = new IO::File;	# font file
    my $sz = -s $fn;	# file size

    $fh->open ($fn) || die ("$fn: $!\n");
    print STDERR ("$fn: Loading font file\n") if $verbose;

    # Read in the font data.
    my $len = 0;
    unless ( ($len = $fh->sysread ($data, 4, 0)) == 4 ) {
	die ("$fn: Expecting $sz bytes, got $len bytes\n");
    }

    $self->{origdataformat} = 'pfa';
    if ( $data eq "\0\1\0\0" ) {
	#### WARNING: This is Unix specific! ####
	my $cmd = $ttftot42 || $PostScript::FontMetrics::ttftot42;
	my $f = _qtfn($fn);
	if ( $cmd ) {
	    $cmd .= " -fc $f 2>>/dev/null |";
	    print STDERR ("$fn: Converting True Type font to Type 42\n")
	      if $verbose;
	}
	else {
	    die ("$fn: Cannot convert True Type font\n");
	}

	$fh->close;
	$type = "t";
	print STDERR ("+ $cmd\n") if $trace;
	$fh->open ($cmd) || die ("$cmd: $!\n");;
	$sz = -1;
	$len = 0;
	$self->{origdataformat} = 'ttf';
    }

    while ( $fh->sysread ($data, 32768, $len) > 0 ) {
	$len = length ($data);
    }
    $fh->close;
    print STDERR ("Read $len bytes from $fn\n") if $trace;
    die ("$fn: Expecting $sz bytes, got $len bytes\n")
      if $sz > 0 && $sz != $len;

    # Make ref.
    $data = \"$data";		#";

    # Convert .pfb encoded font data.
    if ( $$data =~ /^\200[\001-\003]/ ) {
	print STDERR ("$fn: Converting to ASCII format\n") if $verbose;
	$data = $self->_pfb2pfa ($data);
	$self->{origdataformat} = 'pfb';
    }
    # Otherwise, must be straight PostScript.
    elsif ( $$data !~ /^%!/ ) {
	die ("$fn: Not a recognizable font file\n");
    }

    # Normalise line endings.
    $$data =~ s/\015\012?/\n/g;

    $self->{data} = $data;
    $self->{type} = $type if defined $type;

    if ( $$data =~ /^%!FontType(\d+)\n\/(\S+)\n/ ) {
	$self->{type} = $1 unless defined $self->{type};
	$self->{name} = $2;
    }
    elsif ( $$data =~ /\/FontName\s*\/(\S+)/ ) {
	$self->{name} = $1;
    }
    elsif ( $$data =~ /\/FontName\s*\(([^\051]+)\)/ ) {
	$self->{name} = $1;
    }
    if ( $$data =~ /\/FamilyName\s*\/(\S+)/ ) {
	$self->{family} = $1;
    }
    elsif ( $$data =~ /\/FamilyName\s*\(([^\051]+)\)/ ) {
	$self->{family} = $1;
    }
    unless ( defined $self->{type} ) {
	$self->{type} = $1 if $$data =~ /\/FontType\s+(\d+)/;
    }
    $self->{version} = $1 if $$data =~ /\/version\s*\(([^\051]+)\)/;
    $self->{italic} = $1 if $$data =~ /\/ItalicAngle\s+([-+]?\d+)/;
    $self->{fixed} = $1 eq "true"
      if $$data =~ /\/isFixedPitch\s+(true|false)/;
    if ( $$data =~ /\/Weight\s*\/(\S+)/ ) {
	$self->{weight} = $1;
    }
    elsif ( $$data =~ /\/Weight\s*\(([^\051]+)\)/ ) {
	$self->{weight} = $1;
    }
    if ( $$data =~ /\/FontMatrix\s*\[\s*(\d+(?:\.\d*)?)\s+(\d+(?:\.\d*)?)\s+(\d+(?:\.\d*)?)\s+(\d+(?:\.\d*)?)\s+(\d+(?:\.\d*)?)\s+(\d+(?:\.\d*)?)\s*\]/ ) {
	$self->{fontmatrix} = [$1,$2,$3,$4,$5,$6];
    }
    $self;
}

sub _qtfn ($) {
    my $f = shift;
    $f =~ s/([\\'])/'\\$1'/g;
    "'".$f."'";
}

sub _pfb2pfa ($;$) {
    my ($self, $data) = @_;	# NOTE: data is a ref!
    my $newdata = "";		# NOTE: will return a ref

    $data = $self->{data} unless defined $data;

    # Structure of .pfb font data:
    #
    #	( ASCII-segment | Binary-segment )+ EOF-indicator
    #
    #	ASCII-segment: \200 \001 length data
    #	Binary-sement: \200 \002 length data
    #	EOF-indicator: \200 \003
    #
    #	length is a 4-byte little endian 'long'.
    #	data   are length bytes of data.

    my $bin = "";		# accumulated unprocessed binary segments
    my $addbin = sub {		# binary segment processor
	print STDERR ("Processing binary segment, ",
		      length($bin), " bytes\n") if $trace;
	($bin = uc (unpack ("H*", $bin))) =~ s/(.{64})/$1\n/g;
	$newdata .= $bin;
	$newdata .= "\n" unless $newdata =~ /\n$/;
	$bin = "";
    };

    while ( length($$data) > 0 ) {
	my ($type, $info, $seg);

	last if $$data =~ /^\200\003/; # EOF indicator

	# Get font segment.
	die ($self->{file}, ": Invalid font segment format\n")
	  unless ($type, $info) = $$data =~ /^\200([\001-\002])(....)/s;

	my $len = unpack ("V", $info);
	# Can't use next statement since $len may be > 32766.
	# ($seg, $$data) = $$data =~ /^......(.{$len})(.*)/s;
	$seg = substr ($$data, 6, $len);
	$$data = substr ($$data, $len+6);

	if ( ord($type) == 1 ) {	# ASCII segment
	    $addbin->() if $bin ne "";
	    print STDERR ($self->{file}, ": ASCII segment, $len bytes\n")
	      if $trace;
	    $newdata .= $seg;
	}
	else { # ord($type) == 2	# Binary segment
	    print STDERR ($self->{file}, ": Binary segment, $len bytes\n")
	      if $trace;
	    $bin .= $seg;
	}
    }
    $addbin->() if $bin ne "";
    return \$newdata;
}

sub _pfa2pfb ($;$) {
    my ($self, $data) = @_;	# NOTE: data is a ref!

    $data = $self->{data} unless defined $data;

    return \do{"\200\001".pack("V",length($$data)).$$data."\200\003"}
      unless $$data =~ m{(^.*\beexec\s*\n+)
                         ([A-Fa-f0-9\n]+)
                         (\s*cleartomark.*$)}sx;

    my ($pre, $bin, $post) = ($1, $2, $3);
    $bin =~ tr/A-Fa-f0-9//cd;
    $bin = pack ("H*", $bin);
    my $nulls;
    ($bin, $nulls) = $bin =~ /(.*[^\0])(\0+)?$/s;
    $nulls = defined $nulls ? length($nulls) : 0;
    while ( $nulls > 0 ) {
	$post = ("00" x ($nulls > 32 ? 32 : $nulls)) . "\n" . $post;
	$nulls -= 32;
    }

    return \do{"\200\001".pack("V",length($pre)).$pre.
	       "\200\002".pack("V",length($bin)).$bin.
	       "\200\001".pack("V",length($post)).$post.
	       "\200\003" };
}

sub _pfa2asm ($;$) {
    my ($self, $data) = @_;	# NOTE: data is a ref!

    $data = $self->{data} unless defined $data;

    if ( $t1disasm ) {
	my $fn = _qtfn($self->{file});
	#### WARNING: This is Unix specific! ####
	my $cmd = "$t1disasm $fn";
	if ( $self->{type} eq 't' ) {
	    if ( $ttftot42 ) {
		$cmd = "$ttftot42 -fc $fn | $t1disasm";
	    }
	    elsif ( $PostScript::FontMetrics::ttftot42 ) {
		$cmd = "$PostScript::FontMetrics::ttftot42 -fc $fn | $t1disasm";
	    }
	    else {
		die ($self->{file}, ": Cannot convert True Type font\n");
	    }
	}

	print STDERR ("+ $cmd |\n") if $trace;
	my $fh = new IO::File ("$cmd |");
	local ($/);
	my $newdata = <$fh>;
	$fh->close or warn ($cmd, ": return ". sprintf("%x", $?), "\n");
	$newdata =~ s/\015\012?/\n/g;
	return \$newdata;
    }

    return $data
      unless $$data =~ m{(^.*\beexec\s*\n+)
                         ([A-Fa-f0-9\n]+)
                         (\n\s*cleartomark.*$)}sx;

    my ($pre, $bin, $post) = ($1, $2, $3);
    $bin =~ tr/A-Fa-f0-9//cd;
    $bin = pack ("H*", $bin);
    my $nulls;
    ($bin, $nulls) = $bin =~ /(.*[^\0])(\0+)?$/s;
    $nulls = defined $nulls ? length($nulls) : 0;
    while ( $nulls > 0 ) {
	$post = ("00" x ($nulls > 32 ? 32 : $nulls)) . "\n" . $post;
	$nulls -= 32;
    }

    my $newdata = "";

    # Conversion based on an C-program marked as follows:
    # /* Written by Carsten Wiethoff 1989 */
    # /* You may do what you want with this code,
    #    as long as this notice stays in it */

    my $input;
    my $output;
    my $ignore = 4;
    my $buffer = 0xd971;

    while ( length($bin) > 0 ) {
	($input, $bin) = $bin =~ /^(.)(.*)$/s;
	$input = ord ($input);
	$output = $input ^ ($buffer >> 8);
	$buffer = (($input + $buffer) * 0xce6d + 0x58bf) & 0xffff;
	next if $ignore-- > 0;
	$newdata .= pack ("C", $output);
    }

    # End conversion.

    # Cleanup (for display only).
    $newdata =~ s/ \-\| (.+?) (\|-?)\n/" -| <".unpack("H*",$1)."> $2\n"/ges;

    # Concatenate and return.
    $newdata = $pre . $newdata . $post;
    return \$newdata;
}

sub _getglyphnames ($;$) {
    my ($self, $data) = @_;
    my @glyphs = ();

    $data = $self->{data} unless defined $data;

    print STDERR ($self->{file}, ": Getting glyph info\n") if $verbose;

    if ( $self->{format} eq "binary" ) {
	$data = $self->_pfb2pfa ($data);
    }

    if ( $$data =~ m|/CharStrings\s.*\n((?s)(.*))| ) {
	$data = $2;
    }
    else {
	$data = $self->_pfa2asm ($data);
	if ( $$data =~ m|/CharStrings\s.*\n((?s)(.*))| ) {
	    $data = $2;
	}
	else {
	    return undef;
	}
    }

    while ( $data =~ m;((^\d*\s*/([.\w]+))|(\bend\b)).*\n;mg ) {
	last if $1 eq "end";
	push (@glyphs, $3);
    }

    \@glyphs;
}

sub _getencoding ($;$) {
    my ($self, $data) = @_;
    my @glyphs = ();

    print STDERR ($self->{file}, ": Getting encoding info\n") if $verbose;

    $data = $self->{data} unless defined $data;
    $data = $$data;		# deref
    $data =~ s/\n\s*%.*$//mg;	# strip comments

    # Name -> standard encoding.
    return $1 if $data =~ m|/Encoding\s+(\S+)\s+def|;

    # Array -> explicit encoding.
    if ( $data =~ m;/Encoding[\n\s]+\[([^\]]+)\][\n\s]+def;m ) {
	my $enc = $1;
	$enc =~ s|\s*/| |g;
	$enc =~ s/^\s+//;
	$enc =~ s/\s+$//;
	$enc =~ s/\s+/ /g;
	if ( $enc eq $StandardEncoding ) {
	    $enc = "StandardEncoding"
	}
	elsif ( $enc eq $ISOLatin1Encoding ) {
	    $enc = "ISOLatin1Encoding"
	}
	else {
	    $enc = [split (' ', $enc)];
	}
	return $enc;
    }

    # Sparse array, probably custom encoding.
#    if ( $data =~ m;/Encoding \d+ array\n(0 1 .*for\n)?((dup \d+\s*/\S+ put(\s*%.*)?\n)+); ) {
    if ( $data =~ m;/Encoding \d+ array\n(0 1 .*for\n)?((dup \d+\s*/\S+ put(.*)\n)+); ) {
	my $enc = $2;
	my @enc = (".notdef") x 256;
	while ( $enc =~ m;dup (\d+)\s*/(\S+) put;g ) {
	    $enc[$1] = $2;
	}
	if ( "@enc" eq $StandardEncoding ) {
	    $enc = "StandardEncoding"
	}
	elsif ( "@enc" eq $ISOLatin1Encoding ) {
	    $enc = "ISOLatin1Encoding"
	}
	else {
	    $enc = \@enc;
	}
	return $enc;
    }

    undef;
}

sub _getexec ($) {
    my ($exec) = @_;
    foreach ( File::Spec->path ) {
	if ( -x "$_/$exec" ) {
	    print STDERR ("Using $_/$exec\n") if $verbose;
	    return "$_/$exec";
	}
	elsif ( -x "$_/$exec.exe" ) {
	    print STDERR ("Using $_/$exec.exe\n") if $verbose;
	    return "$_/$exec.exe";
	}
    }
    ''
}

1;

__END__

################ Documentation ################

=head1 NAME

PostScript::Font - module to fetch data from PostScript fonts

=head1 SYNOPSIS

  my $info = new PostScript::Font (filename, options);
  print STDOUT ("Name = ", $info->FontName, "\n");

=head1 DESCRIPTION

This package reads PostScript font files and stores the information in memory.

Most font file formats that are in use are recognised, especially the
Type 1 and Type 42 font formats. Other formats that usually parse okay
are Type 5 and Type 3, although Type 3 can sometimes fail depending on
how weird the font information is stored.

The input font file can be encoded in ASCII (so-called C<.pfa>
format), or binary (so-called C<.pfb> format).

True Type fonts are understood as well, they are converted internally
to Type 42 format. Currently this requires an external program,
I<ttftot42>. This program will be used automatically if it can be
located in the execution C<PATH>. Alternatively, you can set the
variable C<$PostScript::Font::ttftot42> (or
C<$PostScript::FontMetrics::ttftot42>) to the name of the actual
program. See also section L<EXTERNAL PROGRAMS>.

=head1 CONSTRUCTOR

=over 4

=item new ( FILENAME [ , OPTIONS ] )

The constructor will read the file and parse its contents.

=back

=head1 OPTIONS

=over 4

=item error => [ 'die' | 'warn' | 'ignore' ]

How errors must be handled. Default is to call die().
In any case, new() returns a undefined result.

=item format => [ 'ascii' | 'pfa' | 'binary' | 'pfb' ]

The format in which the font data is stored.
Default is C<'ascii'>, suitable to be downloaded to a PostScript printer.

=item verbose => I<value>

Prints verbose info if I<value> is true.

=item trace => I<value>

Prints tracing info if I<value> is true.

=back

=head1 INSTANCE METHODS

Each of these methods can return C<undef> if the corresponding
information could not be found in the file.

=over 4

=item FileName

The name of the file, e.g. 'tir_____.pfb'.

=item FontName

The name of the font, e.g. 'Times-Roman'.

=item FamilyName

The family name of the font, e.g. 'Times'.

=item Version

The version of the font, e.g. '001.007'.

=item ItalicAngle

The italicity of the font, e.g. 0 (normal upright fonts) or -16 (italic font).

=item isFixedPitch

Indicates if this font has fixed pitch.

=item Weight

This font weight.

=item FontType

The font type, e.g. '1' for a Type 1 PostScript font, or 't' for a
True Type font.

True Type fonts will be converted to Type 42 internally, but still
have 't' as FontType.

=item FontMatrix

The font matrix as a reference to an anonymous array with the 6 values.
To find the font scale, use

    int(1/$font->FontMatrix->[0])

=item DataFormat

The format in which the data is kept internally. See the B<format> option.

=item FontData

The complete contents of the file, normalised to Unix-style line endings.
It is in the format as returned by the I<dataformat> method.

=item Encoding

This is either one of the strings C<'StandardEncoding'> or
C<'ISOLatin1Encoding'>, or a reference to an array that holds the
encoding. In this case, the array will contain a glyph name (a string)
for each element that is encoded.

B<NOTE:> Getting the encoding information can fail if the way it was
stored in the font is not recognized by the parser. This is most
likely to happen with manually constructed fonts.

=item EncodingVector

Like I<encoding>, but always returns a reference to the encoding
array. In other words, the standard encodings are returned as arrays
as well.

=item FontGlyphs

This returns a reference to an array holding all the names of the
glyphs this font defines, in the order the definitions occur in the
font data.

B<NOTE:> Getting the glyphs information can fail if the way it was
stored in the font is not recognized by the parser. This is most
likely to happen with manually constructed fonts.

Extracting the glyphs can be slow. It can be speeded up by using the
external program I<t1disasm>. This program will be used automatically,
if it can be found in the execution C<PATH>. Alternatively, you can
set the variable C<$PostScript::Font::t1disasm> to point to the
I<t1disasm> program. This does not apply to type 42 fonts, since these
fonts do not require disassembly to get at the glyph list.

=head1 CLASS METHODS

=over 4

=item StandardEncoding

Returns a reference to an array that contains all the glyphs names for
Adobe's Standard Encoding.

=item ISOLatin1Encoding

Returns a reference to an array that contains all the glyphs names for
ISO-8859-1 (ISO Latin-1) encoding.

=back

=head1 EXTERNAL PROGRAMS

Two external programs can be used by this package.

I<ttftot42> is required when True Type fonts must be handled. It is
called as follows:

    ttftot42 -fc filename

This invocation will write the Type 42 version of the True Type font to
standard output.

I<ttftot42> can be found on http://ftp.giga.or.at/pub/nih/ttftot42

An alternative, but not encouraged, way to handle True Type fonts is
by converting them to Type 1 fonts. An excellent converter, I<ttf2pt1>,
can be used for this. To use this program, create a fake I<ttftot42>
script and set variable $PostScript::Font::ttftot42 to the name of
this script. The script should contain something like this:

    $!/bin/sh
    shift	# get rid of '-fc' argument
    exec ttf2pt1 -ef "$@" - 2>/dev/null

The redirection of error output to /dev/null is necessary. since the
program is quite verbose. Unfortunately, error messages will disappear
as well.

I<ttf2pt1> can be found on http://www.netspace.net.au/~mark/ttf2pt1

B<Note:> the resultant Type1 font is not a real conversion, but a
(very similar) imitation of the original True Type font. Small
differences may be noticable depending on the font quality.

I<t1disasm> can be used to speed up the fetching of the list of font
glyphs from Type 1 fonts. It is called as follows:

    t1disasm filename

This invocation will write the disassembled version of the Type 1 font
to standard output. I<t1disasm> is part of the I<t1utils> package that
can be found at http://www.lcdf.org/~eddietwo/type/ and several other
places.

=head1 KNOWN BUGS

Invoking external programs (I<t1disasm>, I<ttftot42>) is guaranteed to
work on Unix only.

=head1 SEE ALSO

=over 4

=item http://www.adobe.com/supportservice/devrelations/PDFS/TN/T1_SPEC.PDF

The specification of the Type 1 font format.

=item http://www.adobe.com/supportservice/devrelations/PDFS/TN/5012.Type42_Spec.pdf

The specification of the Type 42 font format.

=item http://fonts.apple.com/TTRefMan/index.html

The True Type reference manual.

=item http://www.adobe.com/supportservice/devrelations/PDFS/TN/5004.AFM_Spec.pdf

The specification of the Adobe font metrics file format.

=back

=head1 AUTHOR

Johan Vromans, Squirrel Consultancy <jvromans@squirrel.nl>

=head1 COPYRIGHT and DISCLAIMER

This program is Copyright 1993,1999 by Squirrel Consultancy. All
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
