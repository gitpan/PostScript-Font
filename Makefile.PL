# Makefile.PL -- Makefile for PostScript::Fonts
# Author          : Johan Vromans
# Created On      : Mon Feb  8 11:46:56 1999
# Last Modified By: Johan Vromans
# Last Modified On: Tue Jun 19 10:16:36 2001
# Update Count    : 89
# Status          : Stable

# Verify perl version.
require 5.000;
use Config;

use ExtUtils::MakeMaker;

my @scripts = qw(fontsampler font2pfa font2pfb);
my $usrbin = "/usr/bin";
my $installscript = $Config{installscript};

print STDERR <<EOD unless $installscript eq $usrbin;

WARNING: This Makefile will install user accessible scripts.
The location for these scripts is $installscript.
You may consider to pass INSTALLSCRIPT=$usrbin (or some other
convenient location) to "make install".

EOD

WriteMakefile
  (
   NAME         => 'PostScript::Font',
   ($] >= 5.005) ?
   ( AUTHOR	=> 'Johan Vromans (jvromans@squirrel.nl)',
     ABSTRACT	=> 'Modules to get info for/from PostScript fonts' ) : (),
   VERSION      => "1.05",
   PREREQ_PM	=> { 'Getopt::Long' => 2.00, 'IO' => 0 },
   EXE_FILES    => [ map { "script/$_" } @scripts ],
   # *.pm files will be picked up automatically from ./lib
 );
