# Makefile.PL -- Makefile for PostScript::Fonts
# Author          : Johan Vromans
# Created On      : Mon Feb  8 11:46:56 1999
# Last Modified By: Johan Vromans
# Last Modified On: Tue Oct 19 21:32:44 1999
# Update Count    : 72
# Status          : Stable

# Verify perl version.
require 5.000;

use ExtUtils::MakeMaker;

my @scripts = qw(fontsampler font2pfa font2pfb);

WriteMakefile
  (
   NAME         => 'PostScript::Font',
   ($[ >= 5.005) ?
   ( AUTHOR	=> 'Johan Vromans (jvromans@squirrel.nl)',
     ABSTRACT	=> 'Modules to get info from PostScript fonts' ) : (),
   VERSION      => "1.00_02",
   PREREQ_PM	=> { 'Getopt::Long' => 2.00, 'IO' => 0 },
   EXE_FILES    => [ map { "script/$_" } @scripts ],
   # *.pm files will be picked up automatically from ./lib
 );
