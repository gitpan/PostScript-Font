# Makefile.PL -- Makefile for PostScript::Fonts
# Author          : Johan Vromans
# Created On      : Mon Feb  8 11:46:56 1999
# Last Modified By: Johan Vromans
# Last Modified On: Mon Feb  8 21:06:07 1999
# Update Count    : 63
# Status          : Stable

# Verify perl version.
require 5.000;

use ExtUtils::MakeMaker;

my @scripts = qw(fontsampler pfb2pfa pfa2pfb);

WriteMakefile
  (
   NAME         => 'PostScript::Font',
   AUTHOR	=> 'Johan Vromans (jvromans@squirrel.nl)',
   ABSTRACT	=> 'Modules to get info from PostScript fonts',
   VERSION      => "0.02",
   PREREQ_PM	=> { 'Getopt::Long' => 2.00, 'IO' => 0 },
   EXE_FILES    => [ map { "scripts/$_" } @scripts ],
   # *.pm files will be picked up automatically from ./lib
 );
