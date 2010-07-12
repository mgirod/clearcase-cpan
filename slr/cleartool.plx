#!/usr/local/bin/perl -w

use strict;
use warnings;
use vars qw($prog $dieexit $dieexec $diemexec);

# The bulk of the code comes from ClearCase::Wrapper ...
BEGIN {
    # Helpful when discriminating between Windows and good OSes.
    use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i ? 1 : 0;

    # Derive the name we were run as and make it available globally for msgs.
    $prog = $ENV{CLEARCASE_WRAPPER_PROG} || (split m%[/\\]+%, $0)[-1];
    $dieexit = sub { die @_, "\n" };
    $dieexec = sub { die system(@_), "\n" }; #function
    $diemexec = sub { #method: patch after loading
        my $self = shift;
        die $self->system(@_), "\n";
    };
    *Argv::exit = $dieexit;
    *ClearCase::Argv::exit = $dieexit;

    # The "standard" set of overrides supplied with the package.
    # These are autoloaded and thus fairly cheap to read in
    # even though there's lots of code inside.
    if (!$ENV{CLEARCASE_WRAPPER_NATIVE}) {
        *ClearCase::Wrapper::exit = $dieexit;
	*ClearCase::Wrapper::exec = $dieexec;
        require ClearCase::Wrapper;
        {
            no warnings qw(redefine);
            *Argv::exec = $diemexec;
        }
	if ($@) {
	    (my $msg = $@) =~ s%\s*\(.*%!%;
	    warn "$prog: Warning: $msg";
	}
    }
}

sub one_cmd {
    # If Wrapper.pm defines an AutoLoad-ed subroutine to handle $ARGV[0],
    # call it.
    if (!$ENV{CLEARCASE_WRAPPER_NATIVE} && 
            ClearCase::Wrapper::Extension($ARGV[0])) {
	# This provides support for writing extensions.
#	local $^W = 0;
	require ClearCase::Argv;
        {
            no warnings qw(redefine);
            *Argv::exec = $diemexec;
        }
	ClearCase::Argv->VERSION(1.07);
	ClearCase::Argv->attropts; # this is what parses -/dbg=1 et al
	{
	    # "Import" these interfaces in case they're wanted.
	    # Doubled up to suppress a spurious warning.
	    *ClearCase::Wrapper::ctsystem = \&ClearCase::Argv::ctsystem;
	    *ClearCase::Wrapper::ctsystem = \&ClearCase::Argv::ctsystem;
	    *ClearCase::Wrapper::ctexec = \&ClearCase::Argv::ctexec;
	    *ClearCase::Wrapper::ctexec = \&ClearCase::Argv::ctexec;
	    *ClearCase::Wrapper::ctqx = \&ClearCase::Argv::ctqx;
	    *ClearCase::Wrapper::ctqx = \&ClearCase::Argv::ctqx;
	}
	# Convert "ct <cmd> -h" to "ct help <cmd>" for simplicity.
	@ARGV = ('help', $ARGV[0])
	                   if $ARGV[0] ne 'help' && grep(/^-h(elp)?$/, @ARGV);
	# Call the override subroutine ...
	no strict 'refs';
	my $cmd = "ClearCase::Wrapper::$ARGV[0]";
	my $rc = eval { $cmd->(@ARGV) };
	if ($@) {
	    chomp $@; #One extra newline to avoid dumping the stack
	    if ($@ =~ m%^\d+$%) {
	      $rc = $@;
	    } elsif ($@) {
	      print STDERR "$@\n";
	      $rc = 1;
	    } else {
	      $rc = 0;
	    }
	    return $rc; # Completed, successful or not
	}
	# a zero return signals falling back to cleartool
	return $rc if $rc;
    }

    # Either there was no override defined for this command or the override
    # decided to let us finish up by exec-ing the current @ARGV.
    # If we're on Windows we need ClearCase::Argv to avoid the weird
    # behavior of native exec() there. If we're already using ClearCase::Argv
    # we continue to do so, and if any -/foo flags are directed at it
    # we must use it in in order to parse them. But otherwise, so as
    # to not unduly slow down a cmd that isn't being overridden anyway,
    # we skip all that overhead and just exec.
    if ($^O =~ /MSWin32|cygwin/ || defined $Argv::{new} || grep(m%^-/%, @ARGV)) {
	if (grep !m%^-/%, @ARGV) {
	    local $^W = 0;
	    require ClearCase::Argv;
            {
                no warnings qw(redefine);
                *Argv::exec = $diemexec;
            }
            ClearCase::Argv->VERSION(1.43);
	    ClearCase::Argv->attropts;
	    # The -ver flag/cmd is a special case - must be exec-ed.
	    return system('cleartool', @ARGV) if $ARGV[0] =~ /^-ver/i;
	    return ClearCase::Argv->new(@ARGV)->system;
	} else {
	    return system 'cleartool', @ARGV;
	}
    } else {
	if (-d '/opt/rational') {
	    unshift(@ARGV, '/opt/rational/clearcase/bin/cleartool');
	} elsif (-d '/usr/atria') {
	    unshift(@ARGV, '/usr/atria/bin/cleartool');
	} else {
	    unshift(@ARGV, 'cleartool');
	}
	return system(@ARGV);
    }
}
my $status;
if (scalar @ARGV == 1 && $ARGV[0] eq '-status') {
    $status = 1;
    @ARGV = ();
}
if (@ARGV) {
    exit one_cmd;
} else {
    my $rc = 0;
    my $interactive = -t STDIN;
    require Text::ParseWords;
    print "$prog", $status ? $status : '', '> ' if $interactive;
    while (my $line = <>) {
	chomp $line;
	last if $line =~ '^(quit|exit)$';
	local @ARGV = Text::ParseWords::shellwords($line);
	$rc = one_cmd;
	if ($status) {
	    print "Command $status returned status $rc\n";
	    $status++;
	}
	print "$prog", $status?" $status":'', '> ' if $interactive;
    }
    exit $rc;
}
