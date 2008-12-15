use ExtUtils::MakeMaker;
use File::Basename;
use File::Find;

# The upgrade from 0.25 to 1.00 is cleaner if the old version is uninstalled.
# Try to catch that situation here.
my $cnt = 0;
sub wanted {
    return unless /^Burrow.al$/;
    my $d = dirname($File::Find::name);
    return unless $d =~ /Wrapper/;
    return if $cnt++;
    warn qq(Warning: found old version in $d/....
Remove before 'make install' - see README.upgrade for details.\n);
}
find(\&wanted, @INC);

# Explain a little background at install time.
print q(
    This module installs a wrapper script 'cleartool.plx'.  Typically
    you'll want to access it by a shorthand such as 'ct', 'cl', 'clt',
    'Ct', or even just 'c'. This must be set up by hand and may be
    done on UNIX via a symbolic link, e.g.:

	% cd /usr/local/bin; ln -s cleartool.plx ct

    or on Windows by making a tiny ct.bat file containing:

	@echo off
	perl //UNC/path/to/cleartool.plx %*

*NOTE*  Consider installing ClearCase::Wrapper::DSB too.
*NOTE*  As of version 1.00, much of the functionality previously
*NOTE*  in this module has been moved to that one.

);

# The regular stuff.
WriteMakefile(
    'NAME'		=> 'ClearCase::Wrapper',
    'VERSION_FROM'	=> 'Wrapper.pm', # finds $VERSION
    'EXE_FILES'		=> [ 'cleartool.plx' ],
    'PREREQ_PM' => {
	  File::Spec			=> 0.82,
	  Getopt::Long			=> 2.17,
	  ClearCase::Argv		=> 1.06,
	  Argv				=> 1.06,
	  ClearCase::ClearPrompt	=> 1.24,
    },
    'dist' => {
	  DIST_DEFAULT	=> 'all tardist',
	  COMPRESS	=> 'gzip --force --best',
	  TAR		=> 'gtar',
	  TARFLAGS	=> '--mode=u+w -cvf',
    },
);
