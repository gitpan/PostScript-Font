# RCS Status      : $Id: FontInfo.pm,v 1.2 1999-03-07 16:00:53+01 jv Exp $
# Author          : Johan Vromans
# Created On      : December 1999
# Last Modified By: Johan Vromans
# Last Modified On: Sun Mar  7 15:51:24 1999
# Update Count    : 36
# Status          : Looks okay

################ Module Preamble ################

package PostScript::FontInfo;

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
	$self->_loadinfo;
    };

    if ( $@ ) {
	die ($@) unless lc($atts{error}) eq "warn";
	warn ($@);
	return undef;
    }

    $self;
}

sub FileName	{ my $self = shift; $self->{file};    }
sub FontName	{ my $self = shift; $self->{name};    }
sub InfoData	{ my $self = shift; $self->{data};    }
sub FontFamily	{ my $self = shift; $self->{family};  }
sub Version	{ my $self = shift; $self->{version}; }
sub PCFileNamePrefix { my $self = shift; $self->{pcprefix}; }

sub _loadinfo ($) {

    my ($self) = shift;

    my $data;			# inf data

    eval {			# so we can use die

	my $fn = $self->{file};
	my $fh = new IO::File;	# font file
	my $sz = -s $fn;	# file size

	$fh->open ($fn) || die ("$fn: $!\n");
	print STDERR ("$fn: Loading INF file\n") if $verbose;

	# Read in the inf data.
	my $len = 0;
	while ( $fh->sysread ($data, 32768, $len) > 0 ) {
	    $len = length ($data);
	}
	$fh->close;
	print STDERR ("Read $len bytes from $fn\n") if $trace;
	die ("$fn: Expecting $sz bytes, got $len bytes\n") unless $sz == $len;

	# Normalise line endings.
	$data =~ s/\015\012?/\n/g;

	if ( $data !~ /^FontName\s+\(\S+\)$/m ) {
	    die ("$fn: Not a recognizable INF file\n");
	}

    };

    $self->{name}    = $1 if $data =~ /^FontName\s+\((\S+)\)$/mi;
    $self->{family}  = $1 if $data =~ /^FamilyName\s+\((.+)\)$/mi;
    $self->{version} = $1 if $data =~ /^Version\s+\((.+)\)$/mi;
    $self->{pcprefix}= lc($1)
      if $data =~ /^PCFileNamePrefix\s+\((.+)\)$/mi;
    $self->{data}    = $data;

    $self;
}

1;

__END__

################ Documentation ################

=head1 NAME

PostScript::FontInfo - module to fetch data from PostScript font C<.inf> files

=head1 SYNOPSIS

  my $info = new PostScript::FontInfo (filename, options);
  print STDOUT ("Name = ", $info->name, "\n");

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

This package allows font info files, so called C<.inf> files, to be
read and (partly) parsed.

=head1 CONSTRUCTOR

=over 4

=item new ( FILENAME )

The constructor will read the file and parse its contents.

=back

=head1 INSTANCE METHODS

Each of these methods can return C<undef> if the corresponding
information could not be found in the file.

=over 4

=item FileName

The name of the file, e.g. 'tir_____.afm'.

=item FontName

The name of the font, e.g. 'Times-Roman'.

=item FamilyName

The family name of the font, e.g. 'Times'.

=item Version

The version of the font, e.g. '001.007'.

=item PCFileNamePrefix

The prefix used to form MS-DOS compliant file names, e.g. 'tir__'.

=item InfoData

The complete contents of the file, normalised to Unix-style line endings.

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
