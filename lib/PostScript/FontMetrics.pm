# RCS Status      : $Id: FontMetrics.pm,v 1.7 1999-03-07 16:00:53+01 jv Exp $
# Author          : Johan Vromans
# Created On      : December 1999
# Last Modified By: Johan Vromans
# Last Modified On: Sun Mar  7 15:51:14 1999
# Update Count    : 312
# Status          : Released

################ Module Preamble ################

package PostScript::FontMetrics;

use strict;

BEGIN { require 5.005; }

use IO;

use vars qw($VERSION);
$VERSION = "1.0";

my $trace;
my $verbose;

sub new {
    my $class = shift;
    my $font = shift;
    my (%atts) = (error => 'die',
		  verbose => 0, trace => 0,
		  @_);
    my $self = { file => $font };
    bless $self, $class;

    $trace = lc($atts{trace});
    $verbose = $trace || lc($atts{verbose});

    eval {
	$self->_loadafm;
    };

    if ( $@ ) {
	die ($@) unless lc($atts{error}) eq "warn";
	warn ($@);
	return undef;
    }

    $self;
}

sub FileName	{ my $self = shift; $self->{file};    }

sub CharWidthData {
    my $self = shift;
    $self->_getwidthdata() unless defined $self->{Wx};
    $self->{Wx};
}

sub EncodingVector {
    my $self = shift;
    $self->_getwidthdata() unless defined $self->{encodingvector};
    $self->{encodingvector};
}

sub CharBBoxData {
    my $self = shift;
    $self->_getbboxdata() unless defined $self->{BBox};
    $self->{BBox};
}

sub KernData {
    my $self = shift;
    $self->_getkerndata() unless defined $self->{Kern};
    $self->{Kern};
}

sub _loadafm ($) {

    my ($self) = shift;

    my $data;			# afm data

    my $fn = $self->{file};
    my $fh = new IO::File;	# font file
    my $sz = -s $fn;	# file size

    $fh->open ($fn) || die ("$fn: $!\n");
    print STDERR ("$fn: Loading AFM file\n") if $verbose;

    # Read in the afm data.
    my $len = 0;
    while ( $fh->sysread ($data, 32768, $len) > 0 ) {
	$len = length ($data);
    }
    $fh->close;
    print STDERR ("Read $len bytes from $fn\n") if $trace;
    die ("$fn: Expecting $sz bytes, got $len bytes\n") unless $sz == $len;

    # Normalise line endings.
    $data =~ s/\015\012?/\n/g;

    if ( $data !~ /StartFontMetrics/ || $data !~ /EndFontMetrics$/ ) {
	die ("$fn: Not a recognizable AFM file\n");
    }
    $self->{data} = $data;

    # Initially, we only load the "global" info from the AFM data.
    # Other parts are parsed when required.
    local ($_);
    foreach ( split (/\n/, $self->{data}) ) {
	next if /^StartKernData/ .. /^EndKernData/;
	next if /^StartComposites/ .. /^EndComposites/;
	next if /^StartCharMetrics/ .. /^EndCharMetrics/;
	last if /^EndFontMetrics/;
	if ( /^FontBBox\s+(-?\d+)\s+(-?\d+)\s+(-?\d+)\s+(-?\d+)\s*$/ ) {
	    $self->{fontbbox} = [$1,$2,$3,$4];
	}
	elsif ( /(^\w+)\s+(.*)/ ) {
	    my ($key, $val) = ($1, $2);
	    $key = lc ($key);
	    if ( defined $self->{$key}) {
		$self->{$key} = [ $self->{$key} ] unless ref $self->{$key};
		push (@{$self->{$key}}, $val);
	    }
	    else {
		$self->{$key} = $val;
	    }
	}
    }

    $self;
}

sub _getwidthdata {
    # This is adapted from Gisle Aas' Font-AFM 1.17
    my $self = shift;
    local ($_);
    my %wx;
    unless ( defined $self->{encodingvector} ) {
	if ( defined $self->{encodingscheme} ) {
	    if ( $self->{encodingscheme} eq "AdobeStandardEncoding" ) {
		$self->{encodingvector} =
		  [ @{PostScript::Font::StandardEncoding} ];
	    }
	    else {
		$self->{encodingvector} = [];
	    }
	}
    }
    my $enc = $self->{encodingvector};
    my $nglyphs = 0;
    my $nenc = 0;
    foreach ( split (/\n/, $self->{data}) ) {
	if ( /^StartCharMetrics/ .. /^EndCharMetrics/ ) {
	    # Only lines that start with "C" or "CH" are parsed.
	    next unless /^CH?\s+(-?\d+)\s*;/;
	    my $ix = $1;
	    my ($name) = /\bN\s+(\.?\w+)\s*;/;
	    my ($wx)   = /\bWX\s+(\d+)\s*;/;
	    $wx{$name} = $wx;
	    $nglyphs++;
	    $enc->[$ix] = $name, $nenc++ unless $ix < 0;
	    next;
	}
	last if /^EndFontMetrics/;
    }
    unless ( exists $wx{'.notdef'} ) {
	$wx{'.notdef'} = 0;
    }
    print STDERR ($self->FileName, ": Number of glyphs = $nglyphs, ",
		  "encoded = $nenc\n") if $verbose;
    $self->{Wx} = \%wx;
    $self;
}

sub _getbboxdata {
    # This is adapted from Gisle Aas' Font-AFM 1.17
    my $self = shift;
    local ($_);
    my %bbox;
    foreach ( split (/\n/, $self->{data}) ) {
	if ( /^StartCharMetrics/ .. /^EndCharMetrics/ ) {
	    # Only lines that start with "C" or "CH" are parsed.
	    next unless /^CH?\s/;
	    my ($name) = /\bN\s+(\.?\w+)\s*;/;
	    my (@bbox) = /\bB\s+(-?\d+)\s+(-?\d+)\s+(-?\d+)\s+(-?\d+)\s*;/;
	    $bbox{$name} = \@bbox;
	    next;
	}
	last if /^EndFontMetrics/;
    }
    unless ( exists $bbox{'.notdef'} ) {
	$bbox{'.notdef'} = [0, 0, 0, 0];
    }
    $self->{BBox} = \%bbox;
    $self;
}

sub _getkerndata {
    my $self = shift;
    local ($_);
    my %kern;
    foreach ( split (/\n/, $self->{data}) ) {
	if ( /^StartKernData/ .. /^EndKernData/ ) {
	    next unless /^KPX\s+(\S+)\s+(\S+)\s+(-?\d+)/;
	    $kern{$1,$2} = $3;
	}
	last if /^EndFontMetrics/;
    }
    ${kern}{'.notdef','.notdef'} = 0 unless %kern;
    $self->{Kern} = \%kern;
    $self;
}

sub stringwidth {
    my $self = shift;
    my $string = shift;
    my $pt = shift || 1;

    my $wx = $self->CharWidthData;
    my $ev = $self->EncodingVector;
    if ( scalar(@{$self->{encodingvector}}) <= 0 ) {
	die ($self->FileName . ": Missing Encoding\n");
    }
    my $wd = 0;
    foreach ( unpack ("C*", $string) ) {
	$wd += $wx->{$ev->[$_]||'.undef'};
    }
    $wd * $pt / 1000;
}

sub kstringwidth {
    my $self = shift;
    my $string = shift;
    my $pt = shift || 1;

    my $wx = $self->CharWidthData;
    my $ev = $self->EncodingVector;
    if ( scalar(@{$self->{encodingvector}}) <= 0 ) {
	die ($self->FileName . ": Missing Encoding\n");
    }
    my $kr = $self->KernData;
    my $wd = 0;
    my $prev;
    foreach ( unpack ("C*", $string) ) {
	my $this = $ev->[$_] || '.undef';
	$wd += $wx->{$this};
	if ( defined $prev ) {
	    my $kw = $kr->{$prev,$this};
	    $wd += $kw if defined $kw;
	}
	$prev = $this;
    }
    $wd * $pt / 1000;
}

sub AUTOLOAD {
    # This is adapted from Gisle Aas' Font-AFM 1.17
    no strict 'vars';

    if ( $AUTOLOAD =~ /::DESTROY$/ ) {
	eval "sub $AUTOLOAD {}";
	goto &$AUTOLOAD;
    }
    else {
	my $name = $AUTOLOAD;
	$name =~ s/^.*:://;
	return $_[0]->{lc $name};
    }
}

1;

__END__

################ Documentation ################

=head1 NAME

PostScript::FontMetrics - module to fetch data from Adobe Font Metrics file

=head1 SYNOPSIS

  my $info = new PostScript::FontMetrics (filename, options);
  print STDOUT ("Name = ", $info->FontName, "\n");
  print STDOUT ("Width of LAV = ", $info->kstringwidth("LAV", 10), "\n");

=head1 OPTIONS

=over 4

=item error => [ 'die' | 'warn' ]

How errors must be handled.

=item verbose => I<value>

Prints verbose info if I<value> is true.

=item trace => I<value>

Prints tracing info if I<value> is true.

=back

=head1 DESCRIPTION

This package allows Adobe standard font metric files, so called
C<.afm> files, to be read and (partly) parsed.

=head1 CONSTRUCTOR

=over 4

=item new ( FILENAME )

The constructor will read the file and parse its contents.

=back

=head1 INSTANCE METHODS

B<Note:> Most of the info from the AFM file can be obtained by calling a method of the same name, e.g. C<FontName> and C<IsFixedPitch>.

Each of these methods can return C<undef> if the corresponding
information could not be found in the file.

=over 4

=item FileName

The name of the file, e.g. 'tir_____.afm'.
This is not derived from the metrics data, but the name of the file as
passed to the C<new> method.

=item MetricsData

The complete contents of the file, normalised to Unix-style line endings.

=item CharWidthData

Returns a reference to a hash with the character widths for each glyph.

=item EncodingVector

Returns a reference to an array with the glyph names for each encoded
character.

=item CharBBoxData

Returns a reference to a hash with the bounding boxes (a 4-element
array) for each glyph.

=item KernData

Returns a reference to a hash with the kerning data for glyph pairs.
It is indexed by two glyph names (two strings separated by a comma,
e.g. $kd->{"A","B"}).

=item stringwidth ( string [ , pointsize ] )

Returns the width of the string scaled to pointsize.
Default pointsize is 1.

=item kstringwidth ( string [ , pointsize ] )

Returns the width of the string scaled to pointsize, taking kerning
into account.
Default pointsize is 1.

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
