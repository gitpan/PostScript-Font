# PseudoISO.pm -- 
# RCS Info        : $Id: PseudoISO.pm,v 1.2 2000-07-03 17:00:05+02 jv Exp $
# Author          : Johan Vromans
# Created On      : Tue Jun 20 17:07:38 2000
# Last Modified By: Johan Vromans
# Last Modified On: Mon Jul  3 16:59:36 2000
# Update Count    : 33
# Status          : Unknown, Use with caution!

package PostScript::PseudoISO;

$VERSION = "1.00";

use strict;

=head1 NAME

PostScript::PseudoISO - Module with handy ISO enhancements

=head1 SYNOPSYS

    $str = "This is an emdash: ---";
    # Encode
    $t = PostScript::PseudoISO::->prepare($str);

=head1 DESCRIPTION

This module contains some handy extensions to PostScript ISO Latin1
encoding (ISO 8859.1).

The (class) routine C<prepstr> massages a string and makes the
following changes, if appropriate:

=over 4

=item *

Any sequence of three consecutive periods will be changed to render an
ellipses.

=item *

Any sequence of three consecutive dashes will be changed to render an
em-dash.

=item *

Any sequence of two consecutive dashes will be changed to render an
en-dash.

=item *

Straight quotes C<"> and C<'> will be changed to render curly quotes.

=back

=head1 CLASS METHODS

=head2 prepstr

Example:

    $str = PostScript::PseudoISO::->prepstr ("emdash '---'");

This routine makes the changes as described above.

=head2 reencodingvector

Example:

    $vec = PostScript::PseudoISO::->reencodingvector;

This function returns a reference to a hash that contains the mapping
of glyphs that are not part of the ISO Latin1 encoding. This vector
can be used with the C<reencode> method of a
C<PostScript::BasicTypesetter> object.

=cut

sub prepstr {
    my $self = shift;

    # Re-encode a string to (pseudo-)ISO.

    local ($_) = @_;
    my $res = '';
    my $chr;

    # Compress multiple blanks.
    s/\s+/ /g;

    # Handle ellipsis.
    s/(^|[^.])\.\.\.([^.]|$)/$1\200$2/g;

    # Handle em-dash.
    s/(^|[^-])---([^-]|$)/$1\205$2/g;

    # Handle en-dash.
    s/(^|[^-])--([^-]|$)/$1\211$2/g;

    # Handle (pseudo-)quotes.
    while ( /^(.*?)([\"\'])(.*)$/s ) {

	$res .= $1;		# before
	$chr = $2;		# the match
	$_ = $+;		# after

	# Quotes.
	if ( $chr eq '"' ) {
	    # Open quote after space, [ and (; otherwise close quote.
	    $res .= ($res eq '' || $res =~ /[ (\[]$/) ? "\204" : "\202";
	}
	elsif ( $chr eq "'" ) {
	    # Sometimes apostrophe.
	    if ( /^(s-|s |t )/ ) { # 's-Gravenhage, 't, 's nachts
		$res .= "'";
	    }
	    else {
		# Open quote after space, [ and (; otherwise close quote.
		$res .= ($res eq '' || $res =~ /[ (\[]$/) ? "`" : "'";
	    }
	}
    }

    $res . $_;			# return
}

my $reencodingvector;
sub reencodingvector {
    my $self = shift;
    $reencodingvector = {
	"\200" => "ellipsis",
#	"\201" => "quotesingright",
	"\202" => "quotedblright",
#	"\203" => "quotesingleft",
	"\204" => "quotedblleft",
	"\205" => "emdash",
	"\206" => "quotesingle",
	"\207" => "quotedouble",
	"\210" => "bullet",
	"\211" => "endash",
    };

}

1;

__END__

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
