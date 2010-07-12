package ClearCase::Wrapper;

$VERSION = '1.16';

# Require perl 5.6.0, since exists() on a coderef is a new feature in 5.6.
require 5.006;

# NOTES on Debugging addin modules
#
# Include in your modules:
#
# use strict;
# use warnings;
#
#
# NOTES ON AutoLoading and AutoSpliting
#
# Disable the autoloader when debugging.  If the autoloader is running, perl
# has trouble issuing the warnings which can be extremely valuable for
# finding bugs.
#
# To temporaily remove the autoloader, comment out the __END__ directive
# which appears before the functions to be autoloaded, then place the
# following at the end of the module.
#
#   1;
#   __END__
#   sub _AutoSplitDummy {};
#
# When finished debugging, re-enable the autoloader by removing the comment
# on the __END__ directive.  The above stub can be left in the file.
#
# WARNING: Perl can do some crazy things with the symbol table when the module
# contains forward references without prototypes.  This is dealt with
# automatically when using the autoloader, since it creates prototypes. But it
# won't be active when the above is used.  Be AWARE.  Also note that if you use
# strict and warnings as suggested above, perl is pretty good about helping to
# find your problems.
#
#
# NOTES on function names
#
# sub mysub
#   'NORMAL' : 'op' which executes from command line, and help will work.
#
# sub _mysub
#   'HIDDEN' : 'op' which executes from command line, but no help as not
#       	   intended for the user.
# sub Mysub
#   'PUBLIC' : not an 'op', but expected to be shared with other extensions so
#       	    is exported to submodules from WRAPPER
# sub _Mysub
#   'PRIVATE': not an 'op', and not expected to be shared with other extensions.
#

use AutoLoader 'AUTOLOAD';

use strict;
use warnings;

use vars qw(%Packages $libdir $prog $dieexit $dieexec $diemexec);

# Inherit some symbols from the main package. We will later "donate"
# these to all overlay packages as well.
BEGIN {
    *prog = \$::prog;
    *dieexit = \$::dieexit;
    *dieexec = \$::dieexec;
    *diemexec = \$::diemexec;
}

# For some reason this can't be handled the same as $prog above ...
use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i ? 1 : 0;

# Hacks for portability with Windows env vars.
BEGIN {
    $ENV{LOGNAME} ||= $ENV{USERNAME};
    $ENV{HOME} ||= "$ENV{HOMEDRIVE}/$ENV{HOMEPATH}";
}

# Unless the user has their own CLEARCASE_PROFILE, set it to the global one.
BEGIN {
    # Learn where this module was found so we can look there for other files.
    ($libdir = $INC{'ClearCase/Wrapper.pm'}) =~ s%\.pm$%%;

    if (defined $ENV{CLEARCASE_PROFILE}) {
	$ENV{_CLEARCASE_WRAPPER_PROFILE} = $ENV{CLEARCASE_PROFILE};
    } elsif ($ENV{_CLEARCASE_WRAPPER_PROFILE}) {
	$ENV{CLEARCASE_PROFILE} = $ENV{_CLEARCASE_WRAPPER_PROFILE};
    } elsif (! -f "$ENV{HOME}/.clearcase_profile") {
	my $rc = join('/', $libdir, 'clearcase_profile');
	$ENV{CLEARCASE_PROFILE} = $rc if -r $rc;
    }
}

# Skip the Getopt::Long->import(), we need our own GetOptions().
require Getopt::Long;

# Getopt::Long::GetOptions() respects '--' but strips it, while
# we want to respect '--' and leave it in. Thus this override.
sub GetOptions {
    @ARGV = map {/^--$/ ? qw(=--= --) : $_} @ARGV;
    my $ret = Getopt::Long::GetOptions(@_);
    @ARGV = map {/^=--=$/ ? qw(--) : $_} @ARGV;
    return $ret;
}

# Technically we should use Getopt::Long::Configure() for these but
# there's a tangled version history and this is faster anyway.
$Getopt::Long::passthrough = 1; # required for wrapper programs
$Getopt::Long::ignorecase = 0;  # global override for dumb default

# process the exclude file
if (-f "$libdir/exclude") {
    open(FH, "$libdir/exclude") || die $@;
    while (<FH>) {
	my $line = $_;
	chomp;
	s%#.*%%;  # remove comments
	my ($pkg, $op) = ($1, $2) if s%^\s*(\S*)::([^:\s]*)\s*%%;
	if ((!$pkg || !$op) && $_ =~ /\S+/) {
	    warn Msg('W', "Parse error on line $. of $libdir/exclude");
	    warn Msg('W', "  "), $line;
	} elsif ( $pkg && $op && !$_) {
	    Extension( [$op, $pkg, $op, 'excluded'] ) ;
	}
    }
    close FH;
}

# _AutoSplit() allows us to just install .pm files and not the .al files.
# The .al files can be generated here with a -/autosplit cmd line flag.
# In addition we will generate command stems and a complete tree of all
# of the valid aliases for native commands.
{
    my %_AutoSplit;
    _AutoSplit();

    sub _AutoSplit {
	my ($pkg, $ix) = @_;
	return $_AutoSplit{dbg} if $pkg && $pkg eq 'dbg';
	if (! $_AutoSplit{autosplit_init}) {
	    # insure we only evaluate ARGV once
	    $_AutoSplit{autosplit_init} = 1;

	    # search command line for -/autosplit
	    my $prefix = '-/';
	    local $Getopt::Long::genprefix = "($prefix)";
	    GetOptions(\%_AutoSplit, qw(autosplit autospli999));

	    (my $autolibdir = $libdir) =~ s%/ClearCase/Wrapper%/auto%;
	    my $ix = "$autolibdir/ClearCase/Wrapper/autosplit.ix";
	    if (! $_AutoSplit{autosplit} && ! -e $ix) {
		warn Msg('W', "** Missing $ix");
		warn Msg('W', "**  Will perform -/autosplit");
		$_AutoSplit{autosplit} = 1;
	    }
	    if ($_AutoSplit{autosplit}) {
		$_AutoSplit{dbg} = 1 if grep m%^-/dbg%, @ARGV;

		require AutoSplit;
		my @paths = AutoSplit::autosplit("$libdir.pm", $autolibdir);
		if (@paths > 1 || $paths[0]) {
		    # we did an autosplt, read in the newly generated index file
		    require $ix;
		    warn Msg('W', $@) if $@;

		    my $pkg = 'ClearCase::Wrapper';
		    # We did an autosplit, record that fact
		    _AutoSplit($pkg, $ix);

		    # We did an autosplit, let's build the Native hash and
		    # place into the index file for later
		    open  IX, '>>' . $_AutoSplit{$pkg};
		    print IX "\n%Native = qw(\n";
		    for (Native()) {
			printf IX " $_ %s\n", Native($_);
		    }
		    print IX ");\n\n";
		    print IX "\$Native = '$ClearCase::Wrapper::Native';\n";
		    close IX;
		}
	    }
	} else {
	    # return the autosplit status if no parameters
	    return $_AutoSplit{autosplit} if (! $pkg);

	    # if specified, save the autosplit index file name for later
	    if ($ix) {
		$_AutoSplit{$pkg} = $ix;
		return;
	    }
	    # if there is no index file specified, then there was no
	    # autosplit performed.  If no autosplit performed and we read
	    # the autosplit.ix then we are done.
	    no strict 'refs';
	    warn scalar %{"${pkg}::Extension"}, " <- $pkg\n"
		if _AutoSplit('dbg');
	    return if (!$_AutoSplit{$pkg} && scalar %{"${pkg}::Extension"});

	    # We either autosplit or this module is not using the AutoLoader
	    # scan symbols and map any functions that look like ops into
	    # the Extensions hash, then store results
	    _MapExtensions($pkg, $_AutoSplit{$pkg});
	}
    }
}

# Any subroutine declared in a module located via this code
# will eclipse one of the same name declared above.
## NOTE: functions defined in modules found here should not
## be placed directly into ClearCase::Wrapper. They MUST be
## placed in the standard package analogous to their pathname
## (e.g. ClearCase::Wrapper::Foo). Magic occurs here to get
## them into ClearCase::Wrapper where they belong.

sub _FindAndLoadModules {
    my ($dir, $subdir) = @_;
    # Not sure how glob() sorts so force a standard order.
    my @pms = sort glob("$dir/$subdir/*.pm");
    for my $pm (@pms) {
        $pm =~ s%^$dir/(.*)\.pm$%$1%;
        (my $pkg = $pm) =~ s%[/\\]+%::%g;
        # don't load the module twice
        next if grep {$pkg eq $_} keys %Packages;
        # grab argv's functions
        eval "*${pkg}::exit = \$dieexit";
        eval "*${pkg}::exec = \$dieexec";

        # if requested and needed, autosplit the file
        my $pmfile = "$pm.pm";
        my $ix = "$dir/auto/$pm/autosplit.ix";
        if (_AutoSplit()) {
            my @paths = AutoSplit::autosplit("$dir/$pmfile", "$dir/auto");
            if (@paths > 1 || $paths[0]) {
                # if we did an autosplit, record that fact for later
                _AutoSplit($pkg, $ix);
            }
        }

        # In this block we temporarily enter the overlay's package
        # just in case the overlay module forgot its package stmt.
        # Then if it's an autoloaded module (which is recommended),
        # we require the index file and after that require the package.
        # This is because we need to derive a list of all functions
        # defined in the overlay in order to import them to our own
        # namespace.
        {
            local @INC = ($dir);  # make %INC come out right
            if (-e $ix) {
                require $ix;
                warn Msg('W', $@), next if $@;
            } else {
                eval qq(package $pkg); # default the pkg correctly
            }
            require $pmfile;      # load the package
            warn Msg('W', $@), next if $@;
        }

        # if we performed an autosplit, get the command stems and
        # save them to autosplit.ix
        _AutoSplit($pkg);

        # Add to Extension Map the names of extensions defined in
        # this base package.
        {
            no strict 'refs';
            my %exts = %{"${pkg}::Extension"};
            for (keys %exts) {
                Extension( [$_, $pkg, $exts{$_}, ''] );
            }
        }

        # The base module defines a few functions which the overlay's code
        # might want to use. These functions all start with an Uppercase
        # character.  Make aliases for those in the overlay's symbol table.
        for (keys %ClearCase::Wrapper::) {
            next unless /^[A-Z]/;

            # Skip typeglobs that don't involve functions.
            my $tglob = "ClearCase::Wrapper::$_";
            next unless exists &{$tglob};
            eval qq(*${pkg}::$_ = *$tglob);
        }
        eval "*${pkg}::prog = \\\$prog";

        $Packages{$pkg} = $INC{$pmfile};
    }
}
for my $subdir (qw(ClearCase/Wrapper ClearCase/Wrapper/Site)) {
    for my $dir (@INC) {
        _FindAndLoadModules($dir, $subdir)
    }
}

$Packages{'ClearCase::Wrapper'} = __FILE__;

# Piggyback on the -ver flag to show our version too.
if (@ARGV && $ARGV[0] =~ /^-ver/i) {
    my $fmt = "*%-32s %s (%s)\n";
    local $| = 1;
    for (sort keys %Packages) {
	my $ver = eval "\$$_\::VERSION" || '????';
	my $mtime = localtime((stat $Packages{$_})[9]);
	printf $fmt, $_, $ver, $mtime || '----';
    }
    exit 0 if $ARGV[0] =~ /^-verw/i;
}

# Take a string and an array, return the index of the 1st occurrence
# of the string in the array.
sub _FirstIndex {
    my $flag = shift;
    for my $i (0..$#_) {
       return $i if $flag eq $_[$i];
    }
    return undef;
}

# Implements the -me -tag convention (see POD).
if (my $me = _FirstIndex('-me', @ARGV)) {
    if ($ARGV[0] =~ /^(?:set|start|end)view$|^rdl$|^work/) {
	my $delim = 0;
	for (@ARGV) {
	    last if /^--$/;
	    $delim++;
	}
	for (reverse @ARGV[0..$delim-1]) {
	    if (/^\w+$/) {
		$_ = join('_', $ENV{LOGNAME}, $_);
		last;
	    }
	}
	splice(@ARGV, $me, 1);
    } elsif (my $tag = _FirstIndex('-tag', @ARGV)) {
	$ARGV[$tag+1] = join('_', $ENV{LOGNAME}, $ARGV[$tag+1]);
	splice(@ARGV, $me, 1);
    }
}

# Implements the -M flag (see POD).
if (my $mflag = _FirstIndex('-M', @ARGV) || $ENV{CLEARCASE_WRAPPER_PAGER}) {
    splice(@ARGV, $mflag, 1) if $mflag && !$ENV{CLEARCASE_WRAPPER_PAGER};
    pipe(READER, WRITER);
    my $pid;
    if ($pid = fork) {
	close WRITER;
	open(STDIN, ">&READER") || die Msg('E', "STDIN: $!");
	my $pager = $ENV{CLEARCASE_WRAPPER_PAGER} || $ENV{PAGER};
	if (!$pager) {
	    require Config;
	    $pager = $Config::Config{pager} || 'more';
	}
	exec $pager || warn Msg('W', "can't run $pager: $!");
    } else {
	die Msg('E', "can't fork") if !defined($pid);
	close READER;
	open(STDOUT, ">&WRITER") || die Msg('E', "STDOUT: $!");
    }
}

# Implements the -P flag to pause after a GUI operation.
if (my $pflag = _FirstIndex('-P', @ARGV)) {
    splice(@ARGV, $pflag, 1);
    if (MSWIN) {
	eval "END { system qw(cmd /c pause) }";
    } else {
	my $foo = <STDIN>;
    }
}

#############################################################################
# Usage Message Extensions
#############################################################################
{
   no strict 'vars';

   # Extended messages for actual cleartool commands that we extend.
   $checkin     = "\n* [-dir|-rec|-all|-avobs] [-ok] [-diff [diff-opts]]" .
		  "\n* [-revert [-mkhlink]]";
   $checkout    = "\n* [-dir|-rec] [-ok]";
   $diff	= "\n* [-<n>] [-dir|-rec|-all|-avobs]";
   $diffcr      = "\n* [-data]";
   $lsprivate   = "\n* [-dir|-rec|-all] [-ecl/ipsed] [-type d|f]" .
		  "\n* [-rel/ative] [-ext] [dir-pname]";
   $lsview      = "* [-me]";
   $mkelem      = "\n* [-dir|-rec] [-do] [-ok]";
   $uncheckout  = "* [-nc]";

   # Extended messages for pseudo cleartool commands that we implement here.
   $edit	= "<co-flags> [-ci] <ci-flags> pname ...";
   $extensions  = "[-long] [-full]";
}

#############################################################################
# Command Aliases
#############################################################################
*vi     	= *edit;
*ext    	= *extensions;

#############################################################################
# Allow per-user configurability. Give the individual access to @ARGV just
# before we hand it off to the local wrapper function and/or cleartool.
# Access to this feature is suppressed if the 'NO_OVERRIDES' file exists.
#############################################################################
if (-r "$ENV{HOME}/.clearcase_profile.pl" && ! -e "$libdir/NO_OVERRIDES") {
    require "$ENV{HOME}/.clearcase_profile.pl";
    no warnings qw(redefine);
    *Argv::exec = $diemexec;
}

# Add to Extension Map the names of extensions defined in the packages.
{
    # if we performed an autosplit, get the command stems
    _AutoSplit('ClearCase::Wrapper');

    my $exts = \%ClearCase::Wrapper::Extension;
    for (keys %{$exts}) {
	# Add to Extension Map the names of extensions defined in this file
	# unless it was already defined in a submodule, the submodules take
	# precedence.
	my $status = '';
        if (Extension($_)) {
            $status = 'occluded';
            # allow access to extension in Wrapper which are intentionally
            # occluded in other packages
            eval qq(*ClearCase::Wrapper::Occluded::$_ = *ClearCase::Wrapper::$_);
        }
	Extension( [$_, __PACKAGE__, $exts->{$_}, $status] );
    }

    # Map all of the active extensions into our name space
    for (Extension()) {
	my @ext = Extension($_);
	next unless @ext;
	# We import the entire typeglob for 'foo' when we
	# find an extension func named foo(). This allows usage
	# msg extensions (in the form $foo) to come over too.
	my $tglob = "$ext[0]::$ext[1]";
	eval qq(*$_ = *$tglob);
    }
}

# Extension()
#
# If passed an op:
#   Returns undefined if <op> is not being extended.
#   In a scalar context returns the package that extends the op.
#   In an array context returns the package that extends the op and the
#    function name in the package.  The op and the function name may not match
#    because of stem matching and aliasing  (ie uncheck & unco).
#
# If passed no parameter:
#   In a scalar context returns a hash ref to the entire extensions hash
#   In an array context returns the keys to the extension hash.
#     IE: all of the extended ops.
#
# If passed a four element hash reference:
#   Store the elements [op, package, command, action] for later retrieval
{
    my %exts;

    sub Extension {
	my $op = shift;
	if (ref $op) {
	    if (ref $op ne 'ARRAY' || @{$op} != 4) {
		warn Msg('W', ref $op, " $op");
		die "Expected four element ARRAY reference";
	    }
	    if ($op->[3] eq 'excluded') {
		push @{$exts{$op->[0]}->{excluded}}, [$op->[1], $op->[2]];
		return
	    } elsif ($op->[3] eq 'occluded') {
		push @{$exts{$op->[0]}->{occluded}}, [$op->[1], $op->[2]];
		return
	    } elsif ($exts{$op->[0]}->{active}) {
		push @{$exts{$op->[0]}->{occluded}},
		    $exts{$op->[0]}->{active};
	    }
	    for (@{$exts{$op->[2]}->{excluded}}) {
		return if ($_->[0] eq $op->[1] && $_->[1] =~ $op->[0]);
	    }
	    $exts{$op->[0]}->{active} = [$op->[1], $op->[2]];

	} elsif ($op) {
	    return unless ($exts{$op} && $exts{$op}->{active});
	    my $ref = $exts{$op}->{active};
	    return (wantarray ? @{$ref} : $ref->[0]);
	} else {
	    return (wantarray ? keys %exts : \%exts);
	}
    }
}

# This is an enhancement like the ones below but is kept "above the
# fold" because wrapping of cleartool man is an integral and generic
# part of the module. It runs "cleartool man <cmd>" as requested,
# followed by "perldoc ClearCase::Wrapper" iff <cmd> is extended below.
sub man {
    my $page = (grep !/^-/, @ARGV)[1];
    return 0 unless $page;
    ClearCase::Argv->new(@ARGV)->system if Native($page);
    if (exists($ClearCase::Wrapper::{$page})) {
	# This EV hack causes perldoc to search for the right keyword
	# within the module's perldoc.
	if (!MSWIN) {
	    require Config;
	    my $pager = $Config::Config{pager};
	    $ENV{PERLDOC_PAGER} ||= "$pager +/" . uc($page)
		if $pager =~ /more|less/;
	}
    } elsif ($page ne $::prog) {
	if (!Native($page)) {
	    ClearCase::Argv->new(@ARGV)->exec;
	} else {
	    exit($? ? 1 : 0);
	}
    }
    my $psep = MSWIN ? ';' : ':';
    require File::Basename;
    $ENV{PATH} = join($psep, File::Basename::dirname($^X), $ENV{PATH});
    my $module = Extension($page) || __PACKAGE__;
    Argv->perldoc($module)->exec;
    exit $?;
}

1;

__END__

=head1 NAME

ClearCase::Wrapper - General-purpose wrapper for B<cleartool>

=head1 SYNOPSIS

This perl module functions as a wrapper for B<cleartool>, allowing its
command-line interface to be extended or modified. It allows defaults
to be changed, new flags to be added to existing B<cleartool> commands,
or entirely new commands to be synthesized.

=cut

###########################################################################
## Internal service routines, autoloaded since not always needed.
###########################################################################

# Function to read through include files recursively, used by
# config-spec parsing meta-commands. The first arg is a
# filename, the second an "action" which is eval-ed
# for each line.  It can be as simple as 'print' or as
# complex a regular expression as desired. If the action is
# null, only the names of traversed files are printed.
sub Burrow {
    # compatibility with old call signature, throw away uneeded param
    shift if (@_ && $_[0] eq 'CATCS_00')

    my($filename, $action) = @_;
    print $filename, "\n" if !$action;
    open($filename, $filename) || die Msg('E', "$filename: $!");
    while (<$filename>) {
	if (/^include\s+(.*)/) {
	    Burrow($1, $action);
	    next;
	}
	eval $action if $action;
    }
    close($filename);
    return 0;
}

# For standard format error msgs - see code for examples.
sub Msg {
    my $key = shift;
    my $type = {W=>'Warning', E=>'Error'}->{$key} if $key;
    my $msg;
    if ($type) {
	$msg = "$prog: $type: @_";
    } else {
	$msg = "$prog: @_";
    }
    chomp $msg;
    return "$msg\n";
}

# walk back up the stack and find the name of the op.
sub MyOp {
    my $op = '';
    for (my $i=1; ((caller($i))[3]) =~ /ClearCase::Wrapper::/; $i++) {
	$op = (caller($i))[3];
    }
    $op =~ s%.*:%%;
    return $op
}

# Allows the extension writer to make an assertion. If this assertion
# is untrue, dump the current command's usage msg to stderr and exit.
sub Assert {
    my($assertion, @msg) = @_;
    return if $assertion;

    for (@msg) {
	chomp;
	print STDERR Msg('E', $_);
    }
    my $op = MyOp();
    _Helpmsg(\*STDERR, 1, $op) if $op;
}

# Recursive function to find the n'th predecessor of a given version.
sub Pred {
    my($vers, $count, $ct) = @_;
    if ($count) {
	$ct ||= ClearCase::Argv->new;
	(my $elem = $vers) =~ s/@@.*//;
	chomp(my $pred = $ct->desc([qw(-pred -s)], $vers)->qx);
	return Pred("$elem@\@$pred", $count-1, $ct);
    } else {
	return $vers;
    }
}

# Examines supplied arg vector, returns the explicit or implicit working view.
sub ViewTag {
    my $vtag;
    if (@_) {
	local(@ARGV) = @_;
	GetOptions("tag=s" => \$vtag);
    }
    if (!$vtag) {
	require Cwd;
	my $cwd = Cwd::fastgetcwd();
	if (MSWIN) {
	    $cwd =~ s/^[A-Z]://i;
	    $cwd =~ s%\\%/%g;
	}
	if ($cwd =~ m%/+view/([^/]+)%) {
	    $vtag ||= $1;
	}
    }
    if (!$vtag && $ENV{CLEARCASE_ROOT}) {
	$vtag = (split(m%[/\\]%, $ENV{CLEARCASE_ROOT}))[-1];
    }
    $vtag ||= ClearCase::Argv->pwv(['-s'])->qx;
    chomp $vtag if $vtag;
    undef $vtag if $vtag =~ m%\sNONE\s%;
    return $vtag;
}

# Returns the view relative root (IE normally /view or m:)
sub ViewRoot {
    return $ClearCase::Wrapper::ViewRoot if $ClearCase::Wrapper::ViewRoot;

    my $viewroot;
    if (MSWIN()) {
        use vars '%RegHash';
        require Win32::TieRegistry;
        Win32::TieRegistry->import('TiedHash', '%RegHash');
        $viewroot = $RegHash
            {LMachine}->
            {SYSTEM}->
            {CurrentControlSet}->
            {Services}->
            {Mvfs}->
            {Parameters}->
            {drive};

        $viewroot .= ':';
    } else {
        ($viewroot) = grep /viewroot/, Argv->mount({autochomp=>1})->qx;
        $viewroot =~ s%.* on %%;
        $viewroot =~ s% .*%%;
        $viewroot =~ s%([^/])$%$1%;
        $viewroot = "/view" if $viewroot eq "";
    }
    $ClearCase::Wrapper::ViewRoot = $viewroot;
    return $viewroot;
}

# Quicky wrapper for Data::Dumper.  Pass var name as a string
#  ie: _Dump('%myvar', \%myvar);
# 
# optional third parameter specifies minimum dbglevel at which to perform the
# dump.
sub _Dump {
    my ($name, $ref, $dbglevel) = @_;
    my $sysdbglevel = Argv->dbglevel() || 0;
    $dbglevel ||= 0;
    return if ($dbglevel >= $sysdbglevel);

    require Data::Dumper;
    $Data::Dumper::Indent = 1;
    $Data::Dumper::Sortkeys = 1;
    warn "$name: ", Data::Dumper::Dumper($ref);
}

# Print out the list of elements derived as 'eligible', whatever
# that means for the current op.
sub _ShowFound {
    my $ok = shift;
    my $n = @_;
    my $msg;
    if ($n == 0) {
	$msg = Msg(undef, "no eligible elements found");
    } elsif ($n == 1) {
	$msg = Msg(undef, "found 1 file: @_");
    } elsif ($n <= 10) {
	$msg = Msg(undef, "found $n files: @_");
    } else {
	$msg = Msg(undef, "found $n files: @_[0..3] ...");
    }
    print STDERR $msg;
    # Ask if it's OK to continue, exit if no. Generally results from -ok flag.
    if ($ok && $n) {
	(my $op = (caller(2))[3]) =~ s%.*:%%;
	require ClearCase::ClearPrompt;
	my $a = ClearCase::ClearPrompt::clearprompt(
			    qw(proceed -def p -type ok -pro), "Continue $op?");
	exit 0 unless $a == 0;
    }
}

# Return the list of checked-out elements according to the
# -dir/-rec/-all/-avobs flags. Passes the supplied args to
# lsco, returns the result. The first parameter is a boolean
# indicating whether to give the user an "ok to proceed?"
# prompt; this function may exit if the answer is no.
sub AutoCheckedOut {
    my $ok = shift;
    return () unless @_;
    my @args = @_;
    my @auto = grep /^-(?:dir|rec|all|avo)/, @args;
    return @args unless @auto;
    die Msg('E', "mutually exclusive flags: @auto") if @auto > 1;
    my $lsco = ClearCase::Argv->new('lsco', [qw(-cvi -s)],
						    grep !/^-(d|cvi)/, @args);
    $lsco->stderr(0) if grep !/^-/, @args; # in case v-p files are listed
    chomp(my @co = $lsco->qx);
    if (MSWIN) {
	for (@co) { s%\\%/%g }
    }
    _ShowFound($ok, @co);
    exit 0 unless @co;
    return @co;
}

# Return the list of not-checked-out FILE elements according to
# the -dir/-rec flags (-all/-avobs not supported). The first parameter
# is a boolean indicating whether to give the user an "ok to proceed?"
# prompt; this function may exit if the answer is no.
sub AutoNotCheckedOut {
    my $agg = shift;
    my $ok = shift;
    my $fd = shift;
    shift;      # dump the command name (e.g. 'co')
    die Msg('E', "only -dir/-recurse supported: $agg") if $agg =~ /^-a/;
    # First derive a list of all FILE elements under the cwd.
    my @e = ClearCase::Argv->new(qw(find . -typ), $fd, qw(-cvi -nxn -pri))->qx;
    # Chomp and remove any leading "./".
    for (@e) {
	chomp;
	s%^\.[\\/]%%;
    }
    # Turn the list into a hash.
    my %elems = map {$_ => 1} @e;
    # Then, narrow it to elems WITHIN the cwd unless -rec.
    if ($agg !~ /^-rec/) {
	for (keys %elems) {
	    delete $elems{$_} if m%[/\\]%;
	}
    }
    # Remove those which are already checked out to this view.
    if (%elems) {
	my $lsco = ClearCase::Argv->new('lsco', [qw(-cvi -s)]);
	for ($lsco->args(keys %elems)->qx) {
	    chomp;
	    delete $elems{$_};
	}
    }
    # Done: we have a list of all file elems that are not checked out.
    my @not_co = sort keys %elems;
    if (MSWIN) {
	for (@not_co) { s%\\%/%g }
    }
    _ShowFound($ok, @not_co);
    exit 0 unless @not_co;
    return @not_co;
}

# Return the list of view-private files according to the
# -dir/-rec/-all/-avobs flags. Passes the supplied args to
# ct lsp and massages the result. The first param is a boolean
# indicating whether to give the user an "ok to proceed?"
# prompt; this function may exit if the answer is no.
sub AutoViewPrivate {
    my($ok, $do, $scope, $parents, $screen, @pnames) = @_;
    my @vps;
    # Can't use lsprivate in a snapshot view ...
    if (-e '.@@/main/0') {
	my $lsp = Argv->new([$^X, '-S', $0, 'lsp'], [qw(-s -oth), $scope]);
	$lsp->opts($lsp->opts, '-do') if $do;
	chomp(@vps = $lsp->args(@pnames)->qx);
    } else {
	require File::Spec;
	File::Spec->VERSION(0.82);
	die Msg('E', "-do flag not supported in snapshot views") if $do;
	die Msg('E', "$scope flag not supported in snapshot views")
							    if $scope =~ /^-a/;
	my $ls = ClearCase::Argv->ls([qw(-s -view -vis)]);
	$ls->opts($ls->opts, $scope) if $scope =~ /^-r/;
	chomp(@vps = $ls->qx);
	@vps = map {File::Spec->rel2abs($_)} @vps;
    }
    if (MSWIN) {
	for (@vps) { s%\\%/%g }
    }
    # Some v-p files we may not be interested in ...
    @vps = grep !m%$screen%, @vps if $screen;
    @vps = sort @vps;

    if ($parents && @vps && $scope =~ /^-(dir|rec)/) {
	# In case the command was run in a v-p directory, traverse upwards
	# towards the vob root adding parent directories till we reach
	# a versioned dir.
	@pnames = AbsPath({finddirver=>1}, @pnames);
	for (@pnames) {
	    # AbsPath() returns a /./ in the path at the point of the last
	    # directory element.  So if the /./ is present, there is a view
	    # private dir in @pnames.  If any of the pnames are not directory
	    # versions, we need to check in these dirs also.
	    while (m%[/\\]\.[/\\]%) {
		(my $p = $_) =~ s%([/\\])\.[/\\]%$1%;
		push @vps, $p;
		s%[\\/]+[^\\/]+[\\/]*$%%;
	    }
	}
	my %vps = map {$_ => 1} @vps;
	@vps = sort keys %vps;
    }

    _ShowFound($ok, @vps);      # may exit
    exit 0 unless @vps;
    return @vps;
}

# Given a package name, will scan symbols and map any functions that look
# like ops into the package's Extension hash.
sub _MapExtensions {
    my ($pkg, $ix) = @_;
    if (_AutoSplit('dbg')) {
	$ix = "" if !$ix;
	print "_MapExtensions($pkg, $ix)\n";
    }

    # The newly split overlay module was read in. We need to examine its
    # symbol table, determine which functions it defined. and record them
    # into the Extension Hash
    my %names;
    {
	no strict 'refs';
	%names = %{"${pkg}::"};
    }

    my @special = qw(exit exec);
    my %cmds = ();
    for my $op (reverse sort keys %names) {
	# Skip functions that can't be names of valid cleartool ops.
	next if $op =~ /^_?[A-Z]/;

	# Skip functions that are special
	next if grep {$op eq $_} @special;

	# construct a name to the ext in the package
	my $tglob = ($names{$op} ne '-1') ? $names{$op} : "*${pkg}::$op";

	# Skip typeglobs that don't involve functions.
	next unless exists &{$tglob};

	# Take what survives the above tests and create a hash
	# that maps functions to the pkg that defines them.
	no strict 'refs';
	push @{$cmds{\$$tglob}}, $op;
    }

    # create a hash of all op stems to the same command
    my %ext = ();
    for (keys %cmds) {
	my @commands = @{$cmds{$_}};
	# the longest command is assumed to be the 'base' command
	my ($long_cmd) = sort { length($a) < length($b) } @commands;

	for (@commands) {
	    if ($long_cmd =~ /^$_./) {
		# $_ is a stem of $long_cmd
		for (length($_)+1 .. length($long_cmd)-1) {
		    my $stem = substr($long_cmd,0,$_);
		    $ext{$stem} = $long_cmd;
		}
	    }
	    $ext{$_} = $long_cmd;
	}

	# if we occlude the native command let's make sure we occlude
	# the native command for all of the legal variants
	no strict 'refs';
	if (Native($long_cmd)) {
	    if (Native($long_cmd) eq $long_cmd) {
		for my $stem (Native()) {
		    if ($stem ne $long_cmd && Native($stem) eq $long_cmd) {
			$ext{$stem} = $ext{$long_cmd}
		    }
		}
	    } else {
		warn Msg('W', sprintf("'%s' does not match '$long_cmd'",
		    Native($long_cmd)));
	    }
	# if this is not native, verify we have a help string
	} elsif (! ${"${pkg}::$long_cmd"} && $long_cmd !~ /^_/) {
	    warn Msg('W', "No help string for $long_cmd");
	}
    }

    if (%ext) {
	# if this is an autoloaded module, save the extensions for reload
	if ($ix) {
	    open (IX, ">> $ix") || die $@;
	    print IX "\n%Extension = qw(\n";
	    for (sort keys %ext) {
		print IX " $_ $ext{$_}\n";
	    }
	    print IX ");\n\n1;\n";
	    close IX;
	}

	# Save the extension to the Extension hash, since on this pass
	# we will not be reading it from autosplit.ix
	no strict 'refs';
	%{"${pkg}::Extension"} = %ext;
    }
}

# Native()
#
# If passed an op:
#   Return the name of the command that <op> refers to.
#   Returns undefined if <op> is not native to cleartool.
#
# If passed no parameter:
#   In a scalar context returns the top level calling function name if native.
#   In an array context returns the keys to the native hash.
#     IE: all of valid cleartool command op including stems and abbreviations.

sub Native {
    my $op = shift;
    # make a version string from Wrapper & Clearcase versions
    if ( _AutoSplit() && !$ClearCase::Wrapper::NativeVersion ) {
	require ClearCase::Argv;
	my $ct = ClearCase::Argv->new({stderr=>0});
	my ($ver) = grep /Product:/, $ct->hostinfo('-l')->qx;
	$ver =~ /Product:\s*(\S.*)/;
	$ClearCase::Wrapper::NativeVersion =
	    "ClearCase::Wrapper $ClearCase::Wrapper::VERSION - $1";
    }

    # Regenerate %Native hash if it is not in the autosplit file
    #   or it is 'Autosplit time' and versions don't match
    if  (  ! %ClearCase::Wrapper::Native
	|| ! defined($ClearCase::Wrapper::Native)
	|| $ClearCase::Wrapper::NativeVersion
	&& $ClearCase::Wrapper::Native ne $ClearCase::Wrapper::NativeVersion
	)
    {
	warn Msg('W', "Getting native cleartool commands and stems");
	require ClearCase::Argv;
	my $ct = ClearCase::Argv->new({stderr=>0});

	# get all legal commands
	my %native = ();
	my @usg = grep /^Usage:/, $ct->help()->qx;
	for (@usg) {
	    if (/^Usage:\s*\*?\s*(\w+)\s*(\|\s*(\w+))?/) {
		$native{$1} = $1 if $1;
		$native{$3} = $1 if $3;
	    }
	}

	# capture all of the substrings which are legal commands
	for my $cmd (reverse sort keys %native) {
	    for (reverse 1..length($cmd)-1) {
		my $sub = substr $cmd, 0, $_;
		last if exists $native{$sub};
		my ($usg) = grep /^Usage:/, $ct->help($sub)->qx;
		if ($usg && $usg =~ /^Usage:\s*(\w+)\s*/) {
		    $native{$sub} = $1;
		} else {
		    # add a terminal
		    $native{$sub} = '';
		    last;
		}
	    }
	}
	# remove terminals now that the search is complete
	for (keys %native) {
	    delete $native{$_} if !$native{$_};
	}
	%ClearCase::Wrapper::Native = %native;
	$ClearCase::Wrapper::Native = $ClearCase::Wrapper::NativeVersion;
    }

    # if in array context, return all of the keys so they can iterate
    return sort keys %ClearCase::Wrapper::Native if (wantarray && !$op);

    # if in scalar context, return the name of the command
    # implementing the specified or current op;
    return $ClearCase::Wrapper::Native{$op || MyOp()};
}

# Find the absolute paths of elements in Vobs.
#
# Works with links, view extended paths, and windows drive letters.  This
# differs from Cwd::abs_path in a couple of important ways.
# 
# 1) Takes multiple arguments.  This is done for efficiency to
#    possibly limit the number of cleartool calls.
# 
# 2) Returns the 'canonical' name.  IE: the same name that is returned
#    for '-all' and '-avobs' searches.
# 
# 3) Has code that attempts to unravel absolute path vob symbolic links.
#    Absolute path vob symbolic links are broken in subtle and interesting
#    ways.  Made especially fun by the fact that the broken behavior is
#    different on Unix and Windows.
# 
# Can also return indicator of which elements of path are directory versions.
# To return paths with directory versions highlighted:
#
#   AbsPath({finddirver=>1}, @pnames);
#
# The path will be returned as /vobtag/dir1/./dir2/filename, where the text
# to the left of the '.' is a directory element, and text to the right are
# view private directories.
#  
# If {finddirver=>1} is specified and the path passed in is not in a vob,
#   undef is returned.
#
sub AbsPath {

    my %opt = %{shift()} if (@_ != 0 && ref $_[0]);
    require Cwd;
    @_ = Cwd::getcwd() if (! $_[0]);
    my @notfiles = grep {! -l && ! -e} @_;
    Assert (@notfiles == 0, 
        "The following are not files or directories: @notfiles");
    my $sep = MSWIN() ? '\\' : '/';
    my $dbg = Argv->dbglevel() || 0;

    my $abs_path = sub {
        my $abs_path = Cwd::abs_path($_[0]);
        warn "abs_path0: $_[0]\n" if $dbg;
        warn "abs_path1: $abs_path\n" if ($abs_path && $dbg);
        if (!$abs_path) {
            require File::Basename;
            my $basename = File::Basename::basename($_[0]);
            my $dirname  = File::Basename::dirname($_[0]);
            $abs_path = Cwd::abs_path($dirname) . '/' . $basename;
            warn "abs_path2: $abs_path\n" if ($abs_path && $dbg);
        }
        if (! MSWIN() && $_[0] =~ m%^/view/% && $abs_path !~ m%^/view/%) {
            warn Msg('W', 
                "absolute path symbolic link in view relative path: $_[0]");
        }

        return $abs_path;
    };

    # for each parameter, convert to absolute path, cut into subpieces
    # and then build hash of all possible subdir paths.
    my (%paths, @paths, %ranges, @result);
    for (@_) {
	push @result, $abs_path->($_);
        Assert ($result[$#result], "Cannot find absolute path for $_");
	my @path = split '/', $result[$#result];
	$result[$#result] =~ s%/%\\%g if MSWIN();

	my $path = shift @path;
        while (!$path || $path !~ m%[^/]% || $path =~ m%/view$%) {
            $path .= '/' . shift @path;
        }
	$path =~ s%/%\\%g if MSWIN();

	for my $pathpiece (@path) {
	    $path .= $sep . $pathpiece;
	    if (! exists $paths{$path}) {
		# in case we have overlapping paths, we only store each
		# unique path once.
		$paths{$path} = scalar(keys %paths);  # keep array index
		push @paths, $path;
	    }
	    # for each final result, keep the array index of path pieces
	    unshift @{$ranges{$result[$#result]}}, $paths{$path};
	}
    }

    # describe all subpaths, get oid's
    my $ct = ClearCase::Argv->new({autochomp=>1, stderr=>1, readonly=>1});
    my @oids = $ct->desc(qw[-fmt oid:%On\n], @paths)->qx;
    for my $i (0 .. $#oids) {
	if ( $oids[$i] !~ s%^(oid:.*)%$1\@$paths[$i]% ) {
	    $oids[$i] =~ s%.*%%
	}
    }
    _Dump('%paths', \%paths, 1);

    # for each oid, get canonical path, or if softlink, get link text
    my @pnames = $ct->desc(
	qw[-fmt %En-XYZZY-%[object_kind]p-XYZZY-%[slink_text]p\n], @oids)->qx;

    # spin through and validate results, if softlink, construct path and store
    for my $i (0..$#pnames) {
	# convert $pnames[$i] to its canonical pathname in $1
	if ($pnames[$i] !~ s%^(.*)-XYZZY-(.*)-XYZZY-(.*)%$1%) {
	    # didn't find pattern, so
	    #   this path is not a directory version, remove it
	    delete $paths{$paths[$i]};
	} else {
	    # we found the pattern, if $3 is not empty then this is a symlink
	    # Note: if needed $2 contains element type
	    if ($3) {
		my $symlink = $3;
		warn "pnames[$i]: $pnames[$i] $3 symlink\n" if $dbg;
		if ($symlink =~ m%^[\\/]%) {
		    # symbolic link with absolute path!! Eek!!
		    # 
		    # From IBM Docs:
		    # 
		    # VOB symbolic links in UNIX and Linux
		    #  Use relative VOB symbolic links instead of absolute VOB
		    #  symbolic links. Absolute VOB symbolic links require you
		    #  to use absolute pathnames from the VOB tag level; if the
		    #  VOB mount point should change, the link becomes invalid.
		    # 
		    # VOB symbolic links in Windows
		    #  Use relative VOB symbolic links instead of absolute VOB
		    #  symbolic links. Absolute VOB symbolic links require you
		    #  to use absolute pathnames from the view tag level and
		    #  are therefore valid only in the view in which they were
		    #  created.
		    # 
		    #  Note: Although an absolute VOB symbolic link that
		    #  includes the view tag at the beginning works when you
		    #  are in the view, an absolute VOB symbolic link pointing
		    #  to a pathname that begins with a VOB tag (for example,
		    #  cleartool ln \my_vob\file my_link) does not work.

		    if (MSWIN()) {
                        # win abs symlink must be view relative, and since it
                        # is view relative we need to recurse, since the view
                        # maybe different.
			$pnames[$i] = AbsPath('//view' . $symlink);
		    } else {
			$pnames[$i] = $symlink;
		    }
		} else {
		    $pnames[$i] = "$paths[$i]$sep..$sep$symlink" ;
		}
	    }
	}
    }
    _Dump('@result', \@result, 2);
    _Dump('%paths', \%paths, 2);
    _Dump('%ranges', \%ranges, 2);
    _Dump('@pnames', \@pnames, 1);

    # for each parameter, find the absolute path
    my %converted;
    for (@result) {
	my $result = '';
	# walk back up the directory path, looking for a directory version
	for my $i (@{$ranges{$_}}) {
	    if (exists $paths{$paths[$i]}) {
		# if this pname hasn't been converted
		if ( ! $converted{$i} ) {
		    # convert this path to absolute
                    warn "pnames[$i]: $pnames[$i]\n" if $dbg;
		    $pnames[$i] = $abs_path->($pnames[$i]);
		    $pnames[$i] =~ s%/%\\%g if MSWIN();
		    $converted{$i} = 1;
                    warn "pnames[$i]: $pnames[$i]\n" if $dbg;
		}
		my $suffix = substr($_,length($paths[$i]));
		$result = $pnames[$i];
		if ($suffix) {
		    $result .= $sep . '.' if $opt{finddirver};
		    $result .= $suffix;
		}
                if (! MSWIN() && $_ =~ m%^/view/% && $result !~ m%^/view/%) {
                    warn Msg('W', 
                        "absolute path symbolic link in view relative path: $_");
                }
		$_ = $result;
		last;
	    }
	}
	$_ = undef if (!$result && $opt{finddirver});
    }
    warn "AbsPath: @result\n" if $dbg;
    return wantarray ? @result : "@result";
}


=head1 CLEARTOOL ENHANCEMENTS

=over 4

=item * EXTENSIONS

A pseudo-command which lists the currently-defined extensions. Use with
B<-long> to see which overlay module defines each extension. Note that
both extensions and their aliases (e.g. I<checkin> and I<ci>) are
shown.

The B<-full> flag will show the occlusion and exclusion status of each
extension.

=cut

sub extensions {
    my %opt;
    GetOptions(\%opt, qw(short long full));
    $opt{long} = 1 if $opt{full};
    my $exts = Extension();
    for (sort grep !/^_/, keys %{$exts}) {
	if ($exts->{$_}->{active} && $exts->{$_}->{active}->[1] =~ /^$_./) {
	    next; # trim out the stems
	}
	if ($exts->{$_}->{active}) {
	    print "$exts->{$_}->{active}->[0]::" if $opt{long};
	    print $_, "\n";
	}
	next unless $opt{full};
	for (@{$exts->{$_}->{occluded}}) {
	    print "$_->[0]::$_->[1] **OCCLUDED**\n";
	}
	for (@{$exts->{$_}->{excluded}}) {
	    print "$_->[0]::$_->[1] **EXCLUDED**\n";
	}
    }
    exit 0;
}

=item * CI/CHECKIN

Extended to handle the B<-dir/-rec/-all/-avobs> flags. These are fairly
self-explanatory but for the record B<-dir> checks in all checkouts in
the current directory, B<-rec> does the same but recursively down from
the current directory, B<-all> operates on all checkouts in the current
VOB, and B<-avobs> on all checkouts in any VOB.

Extended to allow B<symbolic links> to be checked in (by operating on
the target of the link instead).

Extended to implement a B<-diff> flag, which runs a B<I<diff -pred>>
command before each checkin so the user can review his/her changes
before typing the comment.

Implements a new B<-revert> flag. This causes identical (unchanged)
elements to be unchecked-out instead of being checked in.

Implements a new B<-mkhlink> flag. This works in the context of the
B<-revert> flag and causes any inbound merge hyperlinks to an unchanged
checked-out element to be copied to its predecessor before the unchanged
element is unchecked-out.

Since checkin is such a common operation a special feature is supported
to save typing: an unadorned I<ci> cmd is C<promoted> to I<ci -dir -me
-diff -revert>. In other words typing I<ct ci> will step through each
file checked out by you in the current directory and view,
automatically undoing the checkout if no changes have been made and
showing diffs followed by a checkin-comment prompt otherwise.

=cut

sub checkin {
    # Allows 'ct ci' to be shorthand for 'ct ci -me -diff -revert -dir'.
    push(@ARGV, qw(-me -diff -revert -dir)) if grep(!/^-pti/, @ARGV) == 1;

    # -re999 isn't a real flag, it's to disambiguate -rec from -rev
    # Same for -cr999.
    my %opt;
    GetOptions(\%opt, qw(crnum=s cr999=s diff ok revert re999 mkhlink mk999))
			if grep /^-(crn|dif|ok|rev|mkh)/, @ARGV;

    die Msg('E', "-mkhlink flag requires -revert flag")
			if ($opt{mkhlink} && ! $opt{revert});

    # This is a hidden flag to support my checkin_post trigger.
    # It allows the bug number to be supplied as a cmdline option.
    $ENV{CRNUM} = $opt{crnum} if $opt{crnum};

    my $ci = ClearCase::Argv->new(@ARGV);

    # Parse checkin and (potential) diff flags into different optsets.
    $ci->parse(qw(c|cfile=s cqe|nc
		    nwarn|cr|ptime|identical|rm|cact|cwork from=s));
    if ($opt{diff} || $opt{revert}) {
	$ci->optset('DIFF');
	$ci->parseDIFF(qw(serial_format|diff_format|window columns|options=s
			    graphical|tiny|hstack|vstack|predecessor));
    }

    # Now do auto-aggregation on the remaining args.
    my @elems = AutoCheckedOut($opt{ok}, $ci->args);    # may exit

    # Turn symbolic links into their targets so CC will "do the right thing".
    for (@elems) { $_ = readlink if -l && defined readlink }

# Turned off - on further review this feature seems too intrusive.
=pod
    # Default to -nc if checking in directories only.
    if (!grep(/^-c$|^-cq|^-nc$|^-cfi/, @ARGV)) {
	$ci->opts('-nc', $ci->opts) if !grep {!-d} @elems;
    }
=cut

    # Give a warning if the file is open for editing by vim.
    # (I know, there are lots of other editors but it just happens
    # to be easy to detect vim by its .swp file)
    for my $i (reverse (0..$#elems)) {
        my $elem = $elems[$i];
        if (-f ".${elem}.swp") {
            warn Msg('W', "$elem: appears to be open in vim! skipping checkin");
            splice(@elems,$i,1);
        }
    }
    $ci->args(@elems);

    # Unless -diff or -revert in use, we're done.
    $ci->exec unless $opt{diff} || $opt{revert};

    # Make sure the -pred flag is there as we're going one at a time.
    my $diff = $ci->clone->prog('diff');
    $diff->optsDIFF(qw(-pred -serial), $diff->optsDIFF);

    # In case ~/.clearcase_profile makes ci -nc the default, make sure
    # we prompt for a comment - unless checking in dirs only.
    $ci->opts('-cqe', $ci->opts)
			if !grep(/^-c|^-nc$/, $ci->opts) && grep(-f, @elems);

    # Without -diff we only care about return code
    $diff->stdout(0) unless $opt{diff};

    # With -revert, suppress msgs from typemgrs that don't do diffs
    $diff->stderr(0) if $opt{revert};

    # Now process each element, diffing and then either ci-ing or unco-ing.
    for my $elem (@elems) {
	my $chng = $diff->args($elem)->system('DIFF');
	if ($opt{revert} && !$chng) {
	    # If -revert and -mkhlink and no changes, copy hlinks before unco
	    if ($opt{mkhlink}) {
		my $ct = ClearCase::Argv->new({autochomp=>1});
		my @links = grep {s%^<- %%}
		    $ct->desc(['-s', '-ahl', 'Merge'], $elem)->qx;
		my $pred = Pred($elem,1,$ct);
		$pred = Pred($pred,1,$ct) if $pred =~ m%/0$%;
		for (@links) {
		    $ct->mkhlink(['-unidir','Merge'], $_, $pred)->system;
		}
	    }

	    # If -revert and no changes, unco instead of checkin
	    ClearCase::Argv->unco(['-rm'], $elem)->system;
	} else {
	    $ci->args($elem)->system;
	}
    }

    # All done, no need to return to wrapper program.
    exit $?>>8;
}

=item * CO/CHECKOUT

Extended to handle the B<-dir/-rec> flags. NOTE: the B<-all/-avobs>
flags are disallowed for checkout. Also, directories are not checked
out automatically with B<-dir/-rec>.

=cut

sub checkout {
    for (@ARGV[1..$#ARGV]) { $_ = readlink if -l && defined readlink }
    # If no aggregation flags used, we have no value to add so drop out.
    my @agg = grep /^-(?:dir|rec|all|avo)/, @ARGV;
    return 0 unless @agg;
    die Msg('E', "mutually exclusive flags: @agg") if @agg > 1;

    # Remove the aggregation flag, push the aggregated list of
    # not-checked-out file elements onto argv, and return.
    my %opt;
    GetOptions(\%opt, qw(directory recurse all avobs ok));
    my @added = AutoNotCheckedOut($agg[0], $opt{ok}, 'f', @ARGV);  # may exit
    push(@ARGV, @added);
    return 0;
}

=item * DIFF

Extended to handle the B<-dir/-rec/-all/-avobs> flags.

Improved default: if given just one element and no flags, assume B<-pred>.

Extended to implement B<-n>, where I<n> is an integer requesting that
the diff take place against the I<n>'th predecessor.

=cut

sub diff {
    for (@ARGV[1..$#ARGV]) { $_ = readlink if -l && defined readlink }

    # Allows 'ct diff' to be shorthand for 'ct diff -dir'.
    push(@ARGV, qw(-dir)) if @ARGV == 1;

    my $limit = 0;
    if (my @num = grep /^-\d+$/, @ARGV) {
	@ARGV = grep !/^-\d+$/, @ARGV;
	die Msg('E', "incompatible flags: @num") if @num > 1;
	$limit = -int($num[0]);
    }
    my $diff = ClearCase::Argv->new(@ARGV);
    $diff->parse(qw(options=s serial_format|diff_format|window
		    graphical|tiny|hstack|vstack|predecessor));
    my @args = $diff->args;
    my $auto = grep /^-(?:dir|rec|all|avo)/, @args;
    my @elems = AutoCheckedOut(0, @args);       # may exit
    $diff->args(@elems);
    my @opts = $diff->opts;
    my @extra = ('-serial') if !grep(/^-(?:ser|dif|col|g)/, @opts);
    if ($limit && @elems == 1) {
	$diff->args(Pred($elems[0], $limit, ClearCase::Argv->new), @elems);
    } else {
	push(@extra, '-pred') if ($auto || @elems < 2) && !grep(/^-pre/, @opts);
    }
    $diff->opts(@opts, @extra) if @extra;
    if ($auto && @elems > 1) {
	for (@elems) { $diff->args($_)->system }
	exit $?;
    } else {
	$diff->exec;
    }
}

=item * DIFFCR

Extended to add the B<-data> flag, which compares the I<contents>
of differing elements and removes them from the output if the
contents do not differ.

=cut

sub diffcr {
    my %opt;
    GetOptions(\%opt, qw(data)) if grep m%^-d%, @ARGV;
    # If -data not passed, fall through to regular behavior.
    if ($opt{data}) {
	GetOptions(\%opt, qw(long));
	die Msg('E', "incompatible flags: -data and -long")
	    if exists $opt{long};

	require Digest::MD5;
	my $md51 = Digest::MD5->new;
	my $md52 = Digest::MD5->new;

	my $diffcr = ClearCase::Argv->new(@ARGV);
	my @results = $diffcr->qx;
	my %elems;
	for (@results) {
	    if (m%^([<>]\s+)(.*)@@([/\\]\S*)(.*)%) {
		my($prefix, $elem, $version, $suffix) = ($1, $2, $3, $4);
		next if ! -f $elem;
		if (exists $elems{$elem}) {
		    my $same = 0;
		    if ($elems{$elem}->[0] eq $version) {
			$same = 1;
		    } else {
			my $v1 = join('@@', $elem, $elems{$elem}->[0]);
			my $v2 = join('@@', $elem, $version);

			if (open(V1, $v1) && open(V2, $v2)) {
			    $md51->addfile(*V1);
			    close(V1);
			    my $digest1 = $md51->hexdigest;

			    $md52->addfile(*V2);
			    close(V2);
			    my $digest2 = $md52->hexdigest;

			    if ($digest1 eq $digest2) {
				$same = 1;
			    } else {
				chomp $elems{$elem}->[1];
				$elems{$elem}->[1] .= " [$digest1]\n";
				chomp $_;
				$_ .= " [$digest2]\n";
			    }
			}
		    }
		    if (!$same) {
			print $elems{$elem}->[1];
			print;
		    }
		    delete $elems{$elem};
		} else {
		    $elems{$elem} = [$version, $_];
		}
	    } else {
		print;
	    }
	}
	exit(0);
    }
}

=item * EDIT/VI

Convenience command. Same as 'checkout' but execs your favorite editor
afterwards. Takes all the same flags as checkout, plus B<-ci> to check
the element back in afterwards. When B<-ci> is used in conjunction with
B<-diff> the file will be either checked in or un-checked out depending
on whether it was modified.

The aggregation flags B<-dir/-rec/-all/-avo> may be used, with the
effect being to run the editor on all checked-out files in the named
scope. Example: I<"ct edit -all">.

=cut

sub edit {
    for (@ARGV[1..$#ARGV]) { $_ = readlink if -l && defined readlink }
    # Allows 'ct edit' to be shorthand for 'ct edit -dir -me'.
    push(@ARGV, qw(-dir -me)) if @ARGV == 1;
    my %opt;
    # -c999 isn't a real flag, it's there to disambiguate -c vs -ci
    GetOptions(\%opt, qw(ci c999)) if grep /^-ci$/, @ARGV;
    my $co = ClearCase::Argv->new('co', @ARGV[1..$#ARGV]);
    $co->optset('CI');
    $co->parse(qw(out|branch=s reserved|unreserved|ndata|version|nwarn));
    $co->parseCI(qw(nwarn|cr|ptime|identical|rm from=s c|cfile=s cq|nc diff|revert));
    my $editor = $ENV{WINEDITOR} || $ENV{VISUAL} || $ENV{EDITOR} ||
						    (MSWIN ? 'notepad' : 'vi');
    # Handle -dir/-rec/etc
    if (grep /^-(?:dir|rec|all|avo)/, @ARGV) {
	$co->args(grep -f, AutoCheckedOut(0, $co->args));       # may exit
    }
    my $ed = Argv->new;
    $ed->prog($editor);
    $ed->args($co->args);
    $co->args(grep !-w, $co->args);
    $co->opts('-nc', $co->opts);
    $co->autofail(1)->system if $co->args;
    # Run the editor, check return code.
    $ed->system;
    exit $? unless $opt{ci};
    my $ci = Argv->new([$^X, '-S', $0, 'ci']);
    $ci->opts($co->optsCI);
    $ci->opts('-revert') unless $ci->opts;
    $ci->args($ed->args);
    $ci->exec;
}

# Construct and print help string for one op
sub _HelpHelper {
    my ($op, $FH, $cmdstrings, @text) = @_;

    if (Extension($op)) {
	chomp $text[-1] if @text;
	no strict 'refs';
	if (my $msg = $$op) {
	    chomp $msg;
	    my $indent;
	    if (! @text) {
		# cleartool returned nothing for a help request (!Native)
		@text = ("Usage: * $cmdstrings->{$op} ");
		$indent = $text[0];
		# ** for backward compatibility **
		# remove the op from the help string if present
		$msg =~ /^\s*(\w+)\s+/;
		$msg =~ s%^\s*(\w+)\s+%% if Extension($1);
		$msg =~ s%^\s+%%;
	    } else {
		# Native command
		my $cmdstringre = $cmdstrings->{$op};
		$cmdstringre =~ s%\|%\\|%;
		if ($text[0] !~ /$cmdstringre/s) {
		    # native command with extension shortcut
		    $text[0] =~ s%$op%$cmdstrings->{$op}%;
		    my $xtraindent = length($cmdstrings->{$op}) - length($op);
		    for (@text[1..$#text]) {
			$_ = ' ' x $xtraindent . $_;
		    }
		}
		($indent) = ($text[0] =~ /^(Usage:\s*\w+\s*(\|\s*(\w+))?\s+)/);
	    }
	    $indent = ' ' x (length($indent) - 2);
	    $msg =~ s%\n([^\*])%\n  $1%gs;
	    $msg =~ s%\n%\n$indent%gs;
	    if ($msg =~ /^\n/) {
		$msg =~ s%^\n(.*)%$1\n%s if @text > 1;
	    } elsif ($text[0] =~ m%\n$%) {
		$msg = " $msg\n";
		$text[0] =~ s%\n%%;
	    }
	    # insert the extension text at an appropriate place
	    my $at = 1;
	    if ($text[0] =~ /-graph/ && @text > 1) {
		# in general we don't extend the graphical version
		for my $i (1..$#text) {
		    if ($text[$i] =~ / $op /) {
			$at = $i+1;
			last;
		    }
		}
	    }
	    if ($text[$at-1] =~ m%{%) {
		# don't insert in the middle of a {} set.
		while ($text[$at-1] !~ m%}%) {$at++}
	    }
	    splice @text, $at, 0, $msg;
	} else {
	    @text = ("No help string for $op") if ! @text;
	}
	$text[-1] .= "\n";
    }
    print $FH @text;
}

# help and assert helper function for formatting extensions help
sub _Helpmsg {
    my ($FH, $rc, $op) = @_;

    # get an array of all commands which are aliases for a base command
    my %cmds = ();
    for (sort grep !/^_/, Extension()) {
	my $sub = (Extension($_))[1];
	next if (!$sub);	    # trim out the excludes
	next if ($sub =~ /^$_./);   # trim out the stems
	push @{$cmds{$sub}}, $_;
	$op = $cmds{$sub} if $op && $_ =~ /^$op/;
    }

    # for each base command, build a string joining all aliases w/ '|'
    my %cmdstrings = ();
    for (sort keys %cmds) {
	my @commands = reverse sort @{$cmds{$_}};
	# the longest command is listed first
	@commands = sort { length($a) < length($b) } grep {$_} @commands;
	$cmdstrings{$commands[0]} = join(' | ', @commands);
	$op = $commands[0] if $op && $op eq $cmds{$_};
    }

    # fetch the native cleartool help text
    my @text = ClearCase::Argv->new('help', $op)->stderr(0)->qx;

    if ($op) {
	# help for a single command
	_HelpHelper($op, $FH, \%cmdstrings, @text);
    } else {
	# help for all commands
	push @text, "\n";
	my @subtext = shift @text;
	while (@text) {
	    my $line = shift @text;
	    if ($line =~  /^Usage:/) {
		$op = $subtext[0] =~ (/^Usage:\s*(\w+)\s/) ? $1 : "";
		_HelpHelper($op, $FH, \%cmdstrings, @subtext);
		@subtext = ();
	    }
	    push @subtext, $line;
	}
	$op = $subtext[0] =~ (/^Usage:\s*(\w+)\s/) ? $1 : "";
	_HelpHelper($op, $FH, \%cmdstrings, @subtext);

	my $bars = '='x70;
	print $FH "$bars\n= ClearCase::Wrapper Extensions:\n$bars\n\n";

	# now print the help for each extended command
	for (sort keys %cmdstrings) {
	    no strict 'refs';
	    if (my $msg = $$_) {
		my (@text) = grep {$_} split /\n/, $msg;
		next unless @text;
		my $star = Native($_) ? '' : '* ';
		my $leader = "Usage: $star$cmdstrings{$_}";
		for (@text) {
		    print $FH "$leader $_\n";
		    $leader = ' ' x length($leader);
		}
	    }
	}
    }
    exit $rc
}

# No POD for this one because no options (same as native variant).
sub help {
    # Let cleartool handle any malformed requests.
    my $op = $ARGV[1] || '';
    return 0 if (@ARGV > 2 || $op =~ /^_/ || ! Extension($op) && ! Native($op));
    _Helpmsg(\*STDOUT, 0, $op);
}

=item * LSPRIVATE

Extended to recognize B<-dir/-rec/-all/-avobs>.  Also allows
directories to be specified such that 'ct lsprivate dirname1 dirname2'
restricts output to the directories specified. These directory args may
be used in combination with B<-dir> etc.

The B<-eclipsed> flag restricts output to eclipsed elements.

The flag B<-type d|f> is also supported with the usual semantics (see
cleartool find).

The flag B<-visible> flag ignores files not currently visible in the
view.

The flag B<-nvisible> flag shows only files not currently visible in the
view.

Output is relative to the current or specified directory if the
B<-rel/ative> flag is used.

The B<-ext> flag sorts the output by extension.

=cut

sub lsprivate {

    my @what = (@_) ? grep /^-(dir|rec|all|avo|inv)/, @_ : ();
    Assert(@what <= 1, "mutually exclusive flags: @what");

    my %opt;
    GetOptions(\%opt, qw(
	directory|d
	recurse|r
	all|a
	avobs       av999
	eclipsed    ec999
	ext         ex999
	relative    re999
	type=s      ty999
	visible     vi999
	nvisible    nv999
    ));

    my $lsp = ClearCase::Argv->new({autochomp=>1,readonly=>1}, @ARGV);
    $lsp->parse(qw(
	short|s
	long|l
	co          c999
	do          d999
	invob=s     in999
	other       ot999
	size        si999
	tag=s       ta999
    ));
    my @pnames = $lsp->args();

    Assert(!$opt{relative} || $opt{recurse} || $opt{directory} || @pnames != 0,
	"flag -relative requires either dir-pname, -recurse or -directory");
    Assert(!$opt{relative} || @pnames < 2,
	"flag -relative requires either 0 or 1 dir-pname");
    Assert(!$opt{visible} || !$opt{nvisible},
	"mutually exclusive flags: -visible, -nvisible");

    # Default to -rec but accept -dir, -rec is the default to lsp anyways
    $opt{recurse} = 1 unless @what;

    if (@pnames) {
	# no -invob, -avobs or -all w/ pnames
	@what = grep /^-(all|avo|inv)/, @what;
	Assert(@what == 0, "pnames not allowed with flag(s): @what");
    } else {
	# if -all, add -invob => cwd()
	$lsp->opts($lsp->opts, '-invob', '.') if $opt{all};
	# -relative, -recurse and -directory imply a pname of cwd()
	@pnames = qw(.) if ($opt{relative} || $opt{recurse} || $opt{directory});
    }

    # get the pathnames to lsp against
    my %pnames;
    @pnames{@pnames} = AbsPath({finddirver=>1}, @pnames) if @pnames;

    # AbsPath returns undef for items not in a vob
    my @notinvob = grep {! $pnames{$_} } @pnames;
    Assert (@notinvob == 0, "The following are not in a vob: @notinvob");
    @pnames = values %pnames;

    # get the clearcase version
    my (undef, undef, $ccver) = split /\s+/, ClearCase::Argv->hostinfo()->qx;

    # AbsPath() returns a /./ in the path at the point of the last directory
    # element.  So if the /./ is present, there is a view private dir in @pnames
    # if any of the pnames are not directory versions, we need to treat this
    # like pre ver7, IE, we'll need to do the directory extraction ourselves.
    my $ver7 = (0 == grep {m%[/\\]\.[/\\]%} @pnames) && ($ccver =~ /^7\./);

    # this statement removes the './' marking the directory element
    for (@pnames) {s%([/\\])\.[/\\]%$1%};

    # run the lsprivate command
    my @privs = $lsp->args($ver7 ? @pnames : '')->qx;
    exit $? if $? || !@privs;

    # cc before v7+, we need to prune the results ourselves.
    # or if -dir, show only items in the directory (ie: don't recurse)
    if ((! $ver7 || $opt{directory}) && @pnames) {
	my %t_privs;
	for (sort { length($a) < length($b) } @pnames) {
            my $pname = $_;
	    $pname =~ s%([^/])$%$1/%;   	    # end with /
	    $pname =~ s%[/\\]%\\\\%g if MSWIN();    # \ needs to be \\ for regex
	    my $grep = $opt{directory}
		? "$pname\[^\\\\/]+\$"
		: "(?:^|\\s)$pname";
	    for (grep {/$grep/} @privs) {$t_privs{$_} = 1};
	    if (MSWIN() && $grep =~ s%\w:%%) {
		for (grep {/$grep/} @privs) {$t_privs{$_} = 1};
	    }
	}
	@privs = sort keys %t_privs;
	exit 0 if ! @privs;
    }

    # trim the list to the type visibiity specified
    if ($opt{type} || $opt{visible} || $opt{nvisible}) {
	my @flags;
	push @flags, "-$opt{type}"  if $opt{type};
	push @flags, "-e"           if $opt{visible};
	push @flags, "! -e"         if $opt{nvisible};
	my $flags = join ' && ', @flags;
	@privs = eval("grep {$flags} \@privs")
    }

    # Strip out all results which are not eclipsed. An element
    # is eclipsed if (a) there's a view-private copy,
    # (b) there's also a versioned copy, and (c) it's not checked out.
    if ($opt{eclipsed}) {
	my %coed = ();
	if ($lsp->flag('short')) {
	    %coed = map {chomp; $_ => 1}
		ClearCase::Argv->lsco(qw(-avo -s -cvi))->qx;
	}
	my @t_privs;
	for (@privs) {
	    next if /\s\[checkedout\]/;
	    next unless -e "$_@@/main/0";
	    next if exists $coed{$_};
	    push(@t_privs, $_);
	}
	@privs = @t_privs;
	exit 0 if ! @privs;
    }

    # remove prefixs
    if ($opt{relative}) {
        require File::Spec;
        File::Spec->VERSION(0.82);
        @privs = map {File::Spec->abs2rel($_, $pnames[0])} @privs;
    }

    # sort by extension
    if ($opt{ext}) {
	require File::Basename;
	@privs = map { $_->[0] }
	   sort { "$a->[1]$a->[2]$a->[3]" cmp "$b->[1]$b->[2]$b->[3]" }
	   map  { [$_, (File::Basename::fileparse($_, '\.\w+'))[2,0,1]] }
	   @privs;
    } else {
	@privs = sort @privs;
    }

    for (@privs) { print $_, "\n" }
    exit 0;
}

=item * LSVIEW

Extended to recognize the general B<-me> flag, which restricts the
searched namespace to E<lt>B<username>E<gt>_*.

=cut

sub lsview {
    my @args = grep !/^-me/, @ARGV;
    push(@args, "$ENV{LOGNAME}_*") if @args != @ARGV;
    ClearCase::Argv->new(@args)->autoquote(0)->exec;
}

=item * MKELEM

Extended to handle the B<-dir/-rec> flags, enabling automated mkelems
with otherwise the same syntax as original. Directories are also
automatically checked out as required in this mode. B<Note that this
automatic directory checkout is only enabled when the candidate list is
derived via the B<-dir/-rec> flags>.  If the B<-ci> flag is present,
any directories automatically checked out are checked back in too.

By default, only regular (I<-other>) view-private files are considered
by B<-dir|-rec>.  The B<-do> flag causes derived objects to be made
into elements as well.

If B<-ok> is specified, the user will be prompted to continue after the
list of eligible files is determined.

When invoked in a view-private directory, C<mkelem -dir/-rec> will
traverse up the directory structure towards the vob root until it finds
a versioned dir to work from. Directories traversed during this walk
are added to the list of new elements.

=cut

sub mkelem {
    my %opt;
    GetOptions(\%opt, qw(
	directory|d
	recurse
	all|a
	avobs       av999
	do
	ok          o999
    ));
    die Msg('E', "-all|-avobs flags not supported for mkelem")
					if $opt{all} || $opt{avobs};
    return unless $opt{directory} || $opt{recurse};
    my $scope = $opt{recurse} ? '-rec' : '-dir';

    # Usage: mkelem [-eltype elem-type-name] [-nco | -ci [-ptime]]
    #   	  * [-dir|-rec] [-do] [-ok]
    #   	    [-mkpath] [-master] [-nwarn]
    #   	    [-c comment | -cfile pname | -cq | -cqe | -nc] element-pname ...
    #
    # Usage: mkdir [-nco] [-c comment | -cfile pname | -cq | -cqe | -nc]
    #   	   [-master] dir-pname ...

    my $ct = ClearCase::Argv->new({-autofail=>1}, @ARGV);
    # options for 'comments'
    $ct->extract( 'COM', qw(
	comment|c=s
	cfile=s     cf999
	cquery|cq
	cqeach
	ncomment    n999
    ));
    # options for mkdir
    $ct->extract( 'MKD', qw(
	master      ma999
	eltype      el999
	mkpath      mk999
    ));
    # options for mkelem
    $ct->extract( 'MKE', qw(
	nco
	ci
	ptime       pt999
    ));
    $ct->optsCOM('-nc') if (0 == $ct->optsCOM());
    $ct->optsMKD($ct->optsMKD(), $ct->optsCOM());  # mkdir is mkdir + comments
    $ct->optsMKE($ct->optsMKE(), $ct->optsMKD());  # mkelem is mkelem + mkdir

    my   @badflags = map {"-$_"} grep {$ct->flagCOM($_)} qw(cqeach cquery);
    push @badflags,  map {"-$_"} grep {$ct->flagMKD($_)} qw(eltype mkpath);
    Assert (@badflags == 0, "flag @badflags not compatible with $scope");

    # Derive the list of view-private files to work on. This may exit
    # if no eligibles are found.
    my $re = q%(?:\.(?:n|mv)fs_\d+|\.(?:abe|cmake)\.state|\.(?:swp|tmp))$%;
    my @vps = AutoViewPrivate($opt{ok}, $opt{do}, $scope, 1, $re, $ct->args());

    # If the parent directories of any of the candidates are already
    # versioned elements we may need to check them out.
    require File::Basename;
    my (%seen, %direlems);
    for (@vps) {
	my $d = File::Basename::dirname($_);
	next if ! $d || $seen{$d}++;
	my $lsd = $ct->ls(['-d'], $d)->qx;
	# If no version selector was given it's a view-private dir and
	# will be handled below.
	next unless $lsd =~ /\sRule:\s/;
	# If already checked out, nothing to do.
	next if $lsd =~ /CHECKEDOUT$/;
	# Now we know it's an directory element and needs to be checked out.
	$direlems{$d}++;
    }
    $ct->co(['-nc'], keys %direlems)->system if %direlems;

    # note the cwd for later;
    require Cwd;
    my $cwd = Cwd::cwd();

    # Process candidate directories here, then do files below.
    my (@dirs, @files);
    for my $cand (@vps) {
	if (! -d $cand) {
	    push(@files, $cand);
	    next;
	}
	# Now we know we're dealing with directories.  These must not
	# exist at mkelem time so we move them aside, make
	# a versioned dir, then move all the files from the original
	# back into the new dir (still as view-private files).
	my $tmpdir = "$cand.$$.keep.d";
	die Msg('E', "$cand: $!") if !rename($cand, $tmpdir);
	$ct->autofail(0)->mkdir(($ct->optsMKD()), $cand)->system;
	if ($?) {
	    # if the mkdir fails, put tmpdir back
	    my $rc = $?;
	    die Msg('E', "$tmpdir: $!") if !rename($tmpdir, $cand);
	    exit $rc;
	}
	# flush the view cache if the dir we created was cwd
        if ($cwd eq $cand) {
            $ct->setcs(['-curr'])->system;
            # reassert cwd since it just moved
            chdir $cwd;
        }
	opendir(DIR, $tmpdir) || die Msg('E', "$tmpdir: $!");
	while (defined(my $i = readdir(DIR))) {
	    next if $i eq '.' || $i eq '..';
	    die Msg('E', "$cand/$i: $!") if !rename("$tmpdir/$i", "$cand/$i");
	}
	closedir DIR;
	warn Msg('W', "$tmpdir: $!") unless rmdir $tmpdir;
	# Keep a record of directories to be checked in when done.
	push @dirs, $cand;
    }

    # Now we've made all the directories, do the files in one fell swoop.
    $ct->mkelem(($ct->optsMKE()), @files)->system if grep -f, @files;

    # Last - if the -ci flag was supplied, check in the newly created dir
    # and the existing direlems.
    if ((@dirs || %direlems) && $ct->flagMKE('ci')) {
	$ct->ci($ct->optsCOM(), @dirs, keys %direlems)->system ;
    }

    # Done - don't drop back to main program.
    exit 0;
}

=item * UNCO

Extended to accept (and ignore) the standard comment flags for
consistency with other cleartool cmds.

Extended to handle the -dir/-rec/-all/-avobs flags.

Extended to operate on ClearCase symbolic links.

=cut

sub uncheckout {
    my %opt;
    GetOptions(\%opt, qw(ok)) if grep /^-(dif|ok)/, @ARGV;
    for (@ARGV[1..$#ARGV]) { $_ = readlink if -l && defined readlink }
    my $unco = ClearCase::Argv->new(@ARGV);
    $unco->parse(qw(keep rm cact cwork));
    $unco->optset('IGNORE');
    $unco->parseIGNORE(qw(c|cfile=s cqe|nc));
    $unco->args(sort {$b cmp $a} AutoCheckedOut($opt{ok}, $unco->args));
    $unco->exec;
}

=back

=head1 GENERAL FEATURES

=over 4

=item * symlink expansion

Before processing a checkin or checkout command, any symbolic links on
the command line are replaced with the file they point to. This allows
developers to operate directly on symlinks for ci/co.

=item * -M flag

As a convenience feature, the B<-M> flag runs all output through your
pager. Of course C<"ct lsh -M foo"> saves only a few keystrokes over
"ct lsh foo | more" but for heavy users of shell history the more
important feature is that it preserves the value of ESC-_ (C<ksh -o
vi>) or !$ (csh). The CLEARCASE_WRAPPER_PAGER EV has the same effect.

This may not work on Windows, though it's possible that a sufficiently
modern Perl build and a smarter pager than I<more.com> will do the
trick.

=item * -P flag

The special B<-P> flag will cause C<ct> to I<pause> before finishing.
On Windows this means running the built in C<pause> command. This flag
is useful for plugging I<ClearCase::Wrapper> scripts into the CC GUI.

=item * -me -tag

Introduces a global convenience/standardization feature: the flag
B<-me> in the context of a command which takes a B<-tag view-tag>
causes I<"$LOGNAME"> to be prefixed to the tag name with an
underscore.  This relies on the fact that even though B<-me> is a
native cleartool flag, at least through CC 7.0 no command which takes
B<-tag> also takes B<-me> natively. For example:

    % <wrapper-context> mkview -me -tag myview ...

The commands I<setview, startview, endview, and lsview> also take B<-me>,
such that the following commands are equivalent:

    % <wrapper-context> setview dboyce_myview
    % <wrapper-context> setview -me myview

=back

=head1 CONFIGURABILITY

Various degrees of configurability are supported:

=over 4

=item * Global Enhancements and Extensions

To add a global override called 'cleartool xxx', you could just write a
subroutine 'xxx', place it after the __END__ token in Wrapper.pm, and
re-run 'make install'. However, these changes wcould be lost when a new
version of ClearCase::Wrapper is released, and you'd have to take
responsibility for merging your changes with mine.

Therefore, the preferred way to make site-wide customizations or
additions is to make an I<overlay> module. ClearCase::Wrapper will
automatically include ('require') all modules in the
ClearCase::Wrapper::* subclass. Thus, if you work for C<TLA
Corporation> you should put your enhancement subroutines in a module
called ClearCase::Wrapper::TLA and they'll automatically become
available.

A sample overlay module is provided in the C<./examples> subdir. To
make your own you need only take this sample, change all uses of the
word 'MySite' to a string of your choice, replace the sample subroutine
C<mysite()> with your own, and install. It's a good idea to document
your extension in POD format right above the sub and make the
appropriate addition to the "Usage Message Extensions" section.  Also,
if the command has an abbreviation (e.g. checkout/co) you should add
that to the "Command Aliases" section. See ClearCase::Wrapper::DSB
for examples.

Two separate namespaces are recognized for overlays:
I<ClearCase::Wrapper::*> and I<ClearCase::Wrapper::Site::*>. The intent
is that if your extension is site-specific it should go in the latter
area, if of general use in the former. These may be combined.  For
instance, imagine TLA Corporation is a giant international company with
many sites using ClearCase, and your site is known as R85G. There could
be a I<ClearCase::Wrapper::TLA> overlay with enhancements that apply
anywhere within TLA and/or a I<ClearCase::Wrapper::Site::R85G> for
your people only. Note that since overlay modules in the Site namespace
are not expected to be published on CPAN the naming rules can be less
strict, which is why C<TLA> was left out of the latter module name.

Overlays in the general I<ClearCase::Wrapper::*> namespace are
traversed before I<ClearCase::Wrapper::Site::*>. This allows
site-specific configuration to override more general code. Within each
namespace modules are read in standard ASCII sorted alphabetical
order.

A file named I<exclude> placed in the same directory as Wrapper.pm can
be used to exclude the loading of specific extensions.  The format of
the file is the same as the output of the extensions -long command.
Anything after a '#' will be considered a comment.

All override subroutines are called with @ARGV as their parameter list
(and @ARGV is also available directly of course). The function can do
whatever it likes but it's recommended that I<ClearCase::Argv> be used
to run any cleartool subcommands, and its base class I<Argv> be used to
run other programs. These modules help with UNIX/Windows portability
and debugging, and aid in parsing flags into different categories where
required. See their PODs for full documentation, and see the supplied
extensions for lots of examples.

=item * Personal Preference Setting

As well as allowing for site-wide enhancements to be made in
Wrapper.pm, a hook is also provided for individual users to set their
own defaults.  If the file C<~/.clearcase_profile.pl> exists it will be
read before launching any of the sitewide enhancements. Note that this
file is passed to the Perl interpreter and thus has access to the full
array of Perl syntax. This mechanism is powerful but the corollary is
that users must be experienced with both ClearCase and Perl, and to
some degree with the ClearCase::Wrapper module, to use it. Here's an
example:

    % cat ~/.clearcase_profile.pl
    require ClearCase::Argv;
    Argv->dbglevel(1);
    ClearCase::Argv->ipc(2);

The purpose of the above is to turn on ClearCase::Argv "IPC mode"
for all commands. The verbosity (Argv->dbglevel) is only set to
demonstrate that the setting works. The require statement is used
to ensure that the module is loaded before we attempt to configure it.

=item * Sitewide ClearCase Comment Defaults

This distribution comes with a file called I<clearcase_profile> which
is installed as part of the module. If the user has no
I<clearcase_profile> file in his/her home directory and if
CLEARCASE_PROFILE isn't already set, CLEARCASE_PROFILE will
automatically be pointed at this supplied file. This allows the
administrator to set sitewide defaults of checkin/checkout comment
handling using the syntax supported by ClearCase natively but without
each user needing to maintain their own config file or set their own
EV.

=item * CLEARCASE_WRAPPER_NATIVE

This environment variable may be set to suppress all extensions,
causing the wrapper to behave just like an alias to cleartool, though
somewhat slower.

=back

=head1 DIAGNOSTICS

The flag B<-/dbg=1> prints all cleartool operations executed by the
wrapper to stderr as long as the extension in use was coded with
ClearCase::Argv, which is the case for all supplied extensions.

=head1 INSTALLATION

I recommend you install the I<cleartool.plx> file to some global dir
(e.g. /usr/local/bin), then symlink it to I<ct> or whatever short name
you prefer.  For Windows the strategy is similar but requires a
"ct.bat" redirector instead of a symlink. See "examples/ct.bat" in the
distribution.  Unfortunately, there's no equivalent mechanism for
wrapping GUI access to clearcase.

To install or update a global enhancement you must run "make pure_all
install" - at least that's what I've found to work.  Also, don't forget
to check that the contents of
C<lib/ClearCase/Wrapper/clearcase_profile> are what you want your users
to have by default.

=head1 COPYRIGHT AND LICENSE

Copyright (c) 1997-2006 David Boyce (dsbperl AT boyski.com). All rights
reserved.  This Perl program is free software; you may redistribute it
and/or modify it under the same terms as Perl itself.

=cut

1;
__END__
sub _AutoSplitDummy {};

