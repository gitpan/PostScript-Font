# Makefile.PL -- Makefile for PostScript::Fonts
# Author          : Johan Vromans
# Created On      : Mon Feb  8 11:46:56 1999
# Last Modified By: Johan Vromans
# Last Modified On: Fri May 12 16:26:45 2000
# Update Count    : 74
# Status          : Stable

# Verify perl version.
require 5.000;

use ExtUtils::MakeMaker;

my @scripts = qw(fontsampler font2pfa font2pfb);

WriteMakefile
  (
   NAME         => 'PostScript::Font',
   ($] >= 5.005) ?
   ( AUTHOR	=> 'Johan Vromans (jvromans@squirrel.nl)',
     ABSTRACT	=> 'Modules to get info for/from PostScript fonts' ) : (),
   VERSION      => "1.02",
   PREREQ_PM	=> { 'Getopt::Long' => 2.00, 'IO' => 0 },
   EXE_FILES    => [ map { "script/$_" } @scripts ],
   # *.pm files will be picked up automatically from ./lib
 );
