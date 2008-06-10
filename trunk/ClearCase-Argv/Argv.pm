package ClearCase::Argv;

$VERSION = '1.35';

use Argv 1.22;
use Text::ParseWords;

use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i ? 1 : 0;
my $NUL = MSWIN ? 'NUL' : '/dev/null';

@ISA = qw(Argv);
%EXPORT_TAGS = ( 'functional' => [ qw(ctsystem ctexec ctqx ctqv ctpipe chdir) ] );
@EXPORT_OK = (@{$EXPORT_TAGS{functional}});

*AUTOLOAD = \&Argv::AUTOLOAD;

use strict;

my $class = __PACKAGE__;
my %pidcount;
# For programming purposes we can't allow per-user preferences.
if ($ENV{CLEARCASE_PROFILE}) {
    $ENV{_CLEARCASE_PROFILE} = $ENV{CLEARCASE_PROFILE};
    delete $ENV{CLEARCASE_PROFILE};
}

# Allow EV's setting class data in the derived class to override the
# base class's defaults.There's probably a better way to code this.
for (grep !/^_/, keys %Argv::Argv) {
    (my $ev = uc(join('_', __PACKAGE__, $_))) =~ s%::%_%g;
    $Argv::Argv{$_} = $ENV{$ev} if defined $ENV{$ev};
}

my $ct = 'cleartool';

# Attempt to find the definitive ClearCase bin path at startup. Don't
# try excruciatingly hard, it would take unwarranted time. And don't
# do so at all if running setuid or as root. If this doesn't work,
# the path can be set explicitly via the 'find_cleartool' class method.
if (!MSWIN && ($< == 0 || $< != $>)) {
    $ct = '/usr/atria/bin/cleartool';	# running setuid or as root
} elsif ($ENV{PATH} !~ m%\W(atria|ClearCase)\Wbin\b%i) {
    if (!MSWIN) {
	my $abin = $ENV{ATRIAHOME} ? "$ENV{ATRIAHOME}/bin" : '/usr/atria/bin';
	$ENV{PATH} .= ":$abin" if -d $abin && $ENV{PATH} !~ m%/atria/bin%;
    } else {
	local $^W = 0;
	for (   ($ENV{ATRIAHOME} || '') . "/bin",
		'C:/Program Files/Rational/ClearCase/bin',
		'D:/Program Files/Rational/ClearCase/bin',
		'C:/atria/bin',
		'D:/atria/bin') {
	    if (-d $_ && $ENV{PATH} !~ m%$_%) {
		$ENV{PATH} .= ";$_";
		last;
	    }
	}
    }
}

# Class method to get/set the location of 'cleartool'.
sub find_cleartool { (undef, $ct) = @_ if $_[1]; $ct }

# Override of base-class method to change a prog value of 'foo' into
# qw(cleartool foo). If the value is already an array or array ref
# leave it alone. Same thing if the 1st word contains /cleartool/
# or is an absolute path.
sub prog {
    my $self = shift;
    return $self->SUPER::prog unless @_;
    my $prg = shift;
    if (@_ || ref($prg) || $prg =~ m%^/|^\S*cleartool% || $self->ctcmd) {
	return $self->SUPER::prog($prg, @_);
    } else {
	return $self->SUPER::prog([$ct, parse_line('\s+', 1, $prg)], @_);
    }
}

# Overridden to allow for alternate execution modes.
sub exec {
    $class->new(@_)->exec if !ref($_[0]) || ref($_[0]) eq 'HASH';
    my $self = shift;
    if ($self->ctcmd) {
	exit $self->system(@_) >> 8;
    } elsif ($self->ipc) {
	my $rc = $self->system(@_);
	$rc ||= $self->ipc(0);
	exit($rc);
    } else {
	return $self->SUPER::exec(@_);
    }
}

# Overridden to allow for alternate execution modes.
sub system {
    return $class->new(@_)->system if !ref($_[0]) || ref($_[0]) eq 'HASH';
    my $self = shift;

    return $self->SUPER::system(@_) unless $self->ctcmd || $self->ipc;

    my $envp = $self->envp;
    my($ifd, $ofd, $efd) = ($self->stdin, $self->stdout, $self->stderr);
    $self->stderr(0) unless $efd; #workaround for destructive reading
    $self->args($self->glob) if $self->autoglob;
    my @prog = @{$self->{AV_PROG}};
    shift(@prog) if $prog[0] =~ m%cleartool%;
    my @opts = $self->_sets2opts(@_);
    my @args = @{$self->{AV_ARGS}};
    my @cmd = (@prog, @opts, @args);
    my $dbg = $self->dbglevel;
    $self->_addstats("cleartool @prog", scalar @args) if defined %Argv::Summary;
    $self->warning("cannot close stdin of child process") if $ifd;
    if ($self->noexec && !$self->_read_only) {
	$self->_dbg($dbg, '-', \*STDERR, @cmd);
	return 0;
    }
    open(_O, '>&STDOUT');
    open(_E, '>&STDERR');
    my($outplace, $errplace) = (0, 0);
    if ($self->quiet) {
	$outplace = 1;
    } elsif ($ofd == 2) {
	open(STDOUT, '>&STDERR') || warn "Can't dup stdout";
    } elsif ($ofd !~ m%^\d*$%) {
	open(STDOUT, $ofd) || warn "$ofd: $!";
    } else {
	warn "Warning: illegal value '$ofd' for stdout" if $ofd > 2;
        if ($ofd == 0) {
	    $outplace = -1;
	    open(STDOUT, ">$NUL") if $self->ipc();
	}
    }
    $self->_dbg($dbg, '+>', \*STDERR, @cmd) if $dbg && $self->ctcmd;
    if ($efd == 1) {
	open(STDERR, '>&STDOUT') || warn "Can't dup stderr";
    } elsif ($efd !~ m%^\d*$%) {
	open(STDERR, $efd) || warn "$efd: $!";
    } else {
	warn "Warning: illegal value '$efd' for stderr" if $efd > 2;
	$errplace = -1 if $efd == 0;
    }
    my $rc = 0;
    if ($self->ctcmd) {
	my $ctc = ClearCase::CtCmd->new(outfunc=>$outplace, errfunc=>$errplace);
	if ($envp) {
	    local %ENV = %$envp;
	    $ctc->exec(@cmd);
	} else {
	    $ctc->exec(@cmd);
	}
	$rc = $ctc->status;
    } else {
	$rc = $self->_ipc_cmd(undef, @cmd);
    }
    open(STDOUT, '>&_O'); close(_O);
    open(STDERR, '>&_E'); close(_E);
    print STDERR "+ (\$? == $?)\n" if $dbg > 1;
    $self->fail($self->syfail) if $rc;
    $? = $rc;
    return $rc;
}

# Overridden to allow for alternate execution modes.
sub qx {
    return $class->new(@_)->qx if !ref($_[0]) || ref($_[0]) eq 'HASH';
    my $self = shift;

    return $self->SUPER::qx(@_) unless $self->ctcmd || $self->ipc;

    my($rc, $data, $errors);
    my $dbg = $self->dbglevel;

    my $envp = $self->envp;
    my($ifd, $ofd, $efd) = ($self->stdin, $self->stdout, $self->stderr);
    $self->stderr(0) unless $efd; #workaround for destructive reading
    $self->args($self->glob) if $self->autoglob;
    my @prog = @{$self->{AV_PROG}};
    shift(@prog) if $prog[0] =~ m%cleartool%;
    my @opts = $self->_sets2opts(@_);
    my @args = @{$self->{AV_ARGS}};
    my @cmd = (@prog, @opts, @args);
    $self->_addstats("cleartool @prog", scalar @args) if defined %Argv::Summary;
    $self->warning("cannot close stdin of child process") if $ifd;
    if ($self->noexec && !$self->_read_only) {
	$self->_dbg($dbg, '-', \*STDERR, @cmd);
	return 0;
    }
    $self->_dbg($dbg, '+>', \*STDERR, @cmd) if $dbg && $self->ctcmd;
    if ($self->ctcmd) {
	my $ctc = ClearCase::CtCmd->new;
	if ($envp) {
	    local %ENV = %$envp;
	    ($rc, $data, $errors) = $ctc->exec(@cmd);
	} else {
	    ($rc, $data, $errors) = $ctc->exec(@cmd);
	}
	$data ||= '';
	if ($errors) {
	    if ($efd == 1) {
		$data .= $errors;
	    } elsif ($efd == 2) {
		print STDERR $errors;
	    }
	}
	print STDERR "+ (\$? == $?)\n" if $dbg > 1;
	if (wantarray) {
	    my @data = split /\n/, $data;
	    if (! $self->autochomp) {
		for (@data) { $_ .= "\n" }
	    }
	    $self->unixpath(@data) if MSWIN && $self->outpathnorm;
	    print map {"+ <- $_"} @data if @data && $dbg >= 2;
	    if ($rc) {
		$self->lastresults($rc, @data);
		$self->fail($self->qxfail);
	    }
	    $? = $rc;
	    return @data;
	} else {
	    chomp($data) if $self->autochomp;
	    $self->unixpath($data) if MSWIN && $self->outpathnorm;
	    print "+ <- $data" if $data && $dbg >= 2;
	    if ($rc) {
		$self->lastresults($rc, $data);
		$self->fail($self->qxfail);
	    }
	    $? = $rc;
	    return $data;
	}
    } else {
	my @data = ();
	$rc = $self->_ipc_cmd(\@data, @cmd);
	print STDERR "+ (\$? == $?)\n" if $dbg > 1;
	if (wantarray) {
	    chomp(@data) if $self->autochomp;
	    $self->unixpath(@data) if MSWIN && $self->outpathnorm;
	    if ($rc) {
		$self->lastresults($rc, @data);
		$self->fail($self->qxfail);
	    }
	    $? = $rc;
	    return @data;
	} else {
	    my $data = "@data";
	    chomp($data) if $self->autochomp;
	    $self->unixpath($data) if MSWIN && $self->outpathnorm;
	    if ($rc) {
		$self->lastresults($rc, $data);
		$self->fail($self->qxfail);
	    }
	    $? = $rc;
	    return $data;
	}
    }
}

# Overridden to allow special handling for CtCmd and IPC modes.
sub pipe {
    return $class->new(@_)->pipe if !ref($_[0]) || ref($_[0]) eq 'HASH';

    my $self = shift;
    my $mode;
    if ($self->ctcmd) {
	$mode = 'CtCmd';
    } elsif ($self->ipc) {
        $mode = 'IPC';
    }
    if ($mode) {
	my $otherSelf = $self->clone();
	$self->warning("$mode usage incompatible with pipe - temporarily reverting to plain cleartool") if $self->dbglevel;
	($mode eq 'CtCmd') ? $otherSelf->ctcmd(0) : $otherSelf->ipc(0);
	return $otherSelf->SUPER::pipe(@_);
    } else {
	return $self->SUPER::pipe(@_);
    }
}

# Normalizes a path to Unix style (forward slashes).
sub unixpath {
    my $self = shift;
    $self->SUPER::unixpath(@_);
    # Now apply CC-specific, @@-sensitive transforms to partial lines.
    for my $line (@_) {
	my $fixed = '';
	for (split(m%(\S+@@\S+)%, $line)) {
	    s%\\%/%g if m%^\S+@@\S+$%;
	    $fixed .= $_;
	}
	$line = $fixed;
    }
}

# Attaches to or detaches from a CtCmd object for execution.
sub ctcmd {
    my $self = shift;	# this might be an instance or a classname
    my $level = shift;
    $level = 2 if !defined($level) && !defined(wantarray);
    if ($level) {
	eval { require ClearCase::CtCmd };
	if ($@) {
	    my $msg = $@;
	    if ($level == 2 && $msg =~ m%^(Can't locate \S+)%) {
		$msg = $1;
	    } elsif ($level > 2) {
		die("Error: $msg");
	    }
	    if ($level == 1 || $level == 2) {
		# On Windows, if the real ClearCase::CtCmd is missing hack
		# in our own version that uses CAL directly.
		if (MSWIN) {
		    eval { require Win32::OLE };
		    if ($@) {
			warn("Warning: $msg\n") if $level == 2;
			return undef;
		    } else {
			warn("Warning: $msg, using CAL instead\n")
			    if $level == 2;
			*ClearCase::CtCmd::new = \&_ctcmd_new;
			*ClearCase::CtCmd::status = \&_ctcmd_status;
			*ClearCase::CtCmd::exec = \&_ctcmd_cmd2cal;
			*ClearCase::CtCmd::cleartool = \&_ctcmd_cmd2cal;
			$ClearCase::CtCmd::VERSION = '1.01';
			Win32::OLE->Option(Warn => 0);
		    }
		} else {
		    warn("Warning: $msg, using fork/exec instead\n")
			if $level == 2;
		    return undef;
		}
	    } else {
	      no strict 'refs';
	      $self->{CCAV_CTCMD} = 0;
	      return undef;
	    }
	}
    }
    no strict 'refs';		# because $self may be a symbolic hash ref
    if (defined($level)) {
	if ($level) {
	    ClearCase::CtCmd->VERSION(1.01) if $ClearCase::CtCmd::VERSION;
	    if ($self->ipc) {
		$self->ipc(0);	# shut the ipc down
	    }
	    $self->{CCAV_CTCMD} = 1;
	    # If setting a class attribute, export it to the
	    # env in case we fork a child using ClearCase::Argv.
	    ## NOT SURE WE REALLY WANT THIS IN THIS CASE ...??
	    ## $ENV{CLEARCASE_ARGV_CTCMD} = $self->{CCAV_CTCMD} if !ref($self);
	    return $self;
	} else {					# close up shop
	    delete $self->{CCAV_CTCMD} if exists $self->{CCAV_CTCMD};
	    delete $ENV{CLEARCASE_ARGV_CTCMD}
				if $ENV{CLEARCASE_ARGV_CTCMD} && !ref($self);
	    return $self;
	}
    } else {
	if (!defined($self->{CCAV_CTCMD}) && !defined($class->{CCAV_CTCMD})) {
	    return $ENV{CLEARCASE_ARGV_CTCMD} ? $self : 0;
	}
	return ($self->{CCAV_CTCMD} || $class->{CCAV_CTCMD}) ? $self : undef;
    }
}

sub _ctcmd_new {
    my $object = shift;
    my $this = {};
    %$this = @_;
    bless $this, $object;
    $this->{'status'} = 0;
    return $this;
}

sub _ctcmd_status {
    my $this = shift;
    return $this->{'status'}
}

sub _ctcmd_cmd2cal {
    my $self = ref($_[0]) ? shift : undef;
    # It may make more sense to stash a copy of this object rather than
    # recreate it each time.
    my $ct = Win32::OLE->new('ClearCase.ClearTool');
    # Turn list cmds into a quoted string.
    my $cmd = (@_ == 1) ? $_[0] : join(' ', map {"'$_'"} @_);
    # Send the actual command to CAL and get stdout returned.
    my $out = $ct->CmdExec($cmd);
    # Must reap the return code first, then get stderr IFF retcode != 0.
    my $rc = int Win32::OLE->LastError;
    my $err = $rc ? Win32::OLE->LastError . "\n" : '';
    # Massage the output from CmdExec into cmdline-style format ...
    # Turn the literal CRLF sequence into \n for subsequent chomp/splits.
    for ($out, $err) {
	s/\015?\012/\n/g if $_;
    }
    # Strip the OLE verbosity from any error messages.
    if ($err) {
	$err =~ s%^OLE\s+exception.*%%;
	$err =~ s%^\s*%%s;
	$err =~ s%\s*Win32::OLE.*%\n%s;
    }
    $self->{'status'} = $rc if $self;
    my @results = $rc;
    if (defined($out)) {
        if ($self && exists($self->{outfunc}) && ($self->{outfunc} == 0)) {
	    print STDOUT $out;
        } else {
	    push(@results, $out);
        }
    }
    if ($self && exists($self->{errfunc}) && ($self->{errfunc} == 0)) {
	print STDERR $err;
    } else {
	push(@results, $err);
    }
    if (wantarray) {
	return @results;
    } else {
	$? = $rc if $rc;
	print STDERR $results[2] if defined($results[2]);
	return $results[1];
    }
}

# Starts or stops a cleartool coprocess.
sub ipc {
    my $self = shift;	# this might be an instance or a classname
    no strict 'refs';	# because $self may be a symbolic hash ref
    my $level = shift;
    if (defined($level) && !$level) {
	return 0 unless exists($self->{IPC});
	my $down = $self->{IPC}->{DOWN};
	my $pid = $self->{IPC}->{PID};
	delete $self->{IPC};
	return 0 if --$pidcount{$pid};
	# Send an explicit "exit" command and close up shop.
	print $down "exit\n";
	my $rc = close($down);
	waitpid($pid, 0);
	return $rc || $?;
    } elsif (!defined($level)) {
	return exists($self->{IPC}) ? $self : 0;
    }

    if ($self->ctcmd) {
	$self->ctcmd(0);	# shut down the CtCmd connection
    }
    if (exists($class->{IPC})) {
        if ($self ne $class) {
	    ++$pidcount{$class->{IPC}->{PID}};
	    $self->{IPC}->{PID}  = $class->{IPC}->{PID};
	    $self->{IPC}->{DOWN} = $class->{IPC}->{DOWN};
	    $self->{IPC}->{BACK} = $class->{IPC}->{BACK};
	}
	return $self;
    }
    # This should never fail to load since it's built in.
    require IPC::Open3;

    my $ct = MSWIN ?
	'cleartool' :
	join('/', $ENV{ATRIAHOME}||'/opt/rational/clearcase', 'bin/cleartool');

    # Dies on failure.
    my($down, $back);
    my $pid = IPC::Open3::open3($down, $back, undef, $ct);

    # Set the "line discipline" to convert CRLF to \n.
    binmode $back, ':crlf';

    $self->{IPC}->{DOWN} = $down;
    $self->{IPC}->{BACK} = $back;
    $self->{IPC}->{PID} = $pid;
    ++$pidcount{$pid};

    return $self;
}

sub _ipc_cmd {
    my $self = shift;
    my $disposition = shift;

    # Handle verbosity.
    my $dbg = $self->dbglevel;
    $self->_dbg($dbg, '=>', \*STDERR, @_) if $dbg;

    # Send the command to cleartool.
    my $cmd = "@_";
    chomp $cmd;
    my $down = $self->{IPC}->{DOWN};
    print $down $cmd, "\n";

    # Supply custom comment on standard input if requested.
    if (exists $self->{IPC}->{COMMENT}) {
	my $input = $self->{IPC}->{COMMENT};
	print $down $input, "\n.\n";
	delete $self->{IPC}->{COMMENT};
    }
    # Hack to simulate 'cleartool -status', until ClearCase bug fix
    print $down "des -fmt \"Command 0 returned status 0\\n\" .\n";

    # Read back the results and get command status.
    my $rc = 0;
    my $back = $self->{IPC}->{BACK};
    while($_ = <$back>) {
        my ($last, $next);
	my $out = *STDOUT;
	if (m%^cleartool: (Error|Warning):%) {      #Simulate -status
	  $rc += 1 << 8 if $1 == 'Error';
	  if ($self->stderr) {
	      $out = *STDERR;
	  } else {
	      $self->stderr(0);
	      $next = 1 unless $self->stderr;
	  }
	}
	if (s%^(.*)Command \d+ returned status (\d+)%$1%) {
	    # Shift the status up so it looks like an exit status.
	    # $rc = $2 << 8;                        #-status disabled
	    chomp;
	    $_ ? $last = 1 : last;
	}
	print '+ <=', $_ if $_ && $dbg >= 2;
	next if $next;
	if ($disposition) {
	    push(@$disposition, $_);
	} else {
	    print $out $_;
	}
	last if $last;
    }

    return $rc;
}

sub fork_exec {
    my $self = shift;
    $self->ipc(0);
    $self->ctcmd(0);
}

sub exec_style {
    my $self = shift;
    my $style = shift;
    if ($style) {
	no strict 'subs';
	$self->$style(shift || 3);
    }
    if ($self->ctcmd) {
	return "CTCMD";
    } elsif ($self->ipc) {
	return "IPC";
    } else {
	return "FORK";
    }
}

sub _read_only {
    my $self = shift;
    if ($self->readonly =~ /^a/i) {	# a=automatic
	my @cmd = $self->prog;
	if ($cmd[-1] =~ m%^(ls|annotate|apropos|cat|des|diff|dospace|
			    file|getcache|getlog|help|host|man|pw|
			    setview|space)%x) {
	    return 1;
	} else {
	    return 0;
	}
    } else {
	return $self->SUPER::_read_only;
    }
}

# The cleartool command has quoting rules different from any system
# shell so we subclass the quoting method to deal with it. Not
# currently well tested with esoteric cmd lines such as mkattr.
## THIS STUFF IS REALLY COMPLEX WITH ALL THE PERMUTATIONS
## OF PLATFORMS, SUBCLASSES, SHELLS, AND APIs. WATCH OUT.
sub quote {
    my $self = shift;
    # Don't quote the 2nd word where @_ = ('cleartool', 'pwv -s');
    return @_ if @_ == 2;
    return $self->SUPER::quote(@_) if @_ > 2;
    # Ok, now we're looking at interactive-cleartool quoting ("man cleartool").
    if (!MSWIN && $self->ipc) {
	# Special case - extract multiline comments from the cmd line and
	# put them in the stdin stream when using UNIX co-process model.
	for (my $i=0; $i < $#_; $i++) {
	    if ($_[$i] eq '-c' && $_[$i+1] =~ m%\n%s) {
		$self->stdin("$_[$i+1]\n.");
		splice(@_, $i, 2, '-cq');
		last;
	    }
	}
    }
    # Single quotes aren't understood by the &*&#$ Windows shell
    # but cleartool gets them right so this quoting is simpler.
    my $inpathnorm = $self->inpathnorm;
    for (@_) {
	# If requested, change / for \ in Windows file paths.
	s%/%\\%g if $inpathnorm;
	# Now quote embedded quotes ...
	s%'%\\'%g;
	# and then the entire string.
	$_ = qq('$_');
    }
    return @_;
}

# Hack - allow a comment to be registered here. The next command will
# see it with -c "comment" if in regular mode or with -cq and reading
# the comment from stdin if in ->ipc mode.
sub comment {
    my $self = shift;
    my $cmnt = join("\n", @_);
    my @prev = $self->opts;
    if ($self->ipc) {
	$self->opts('-cq', @prev) if !grep /^-cq/, @prev;
	$self->{IPC}->{COMMENT} = $cmnt;
    } else {
	$self->opts('-c', $cmnt, @prev) if !grep /^-c/, @prev;
    }
    return $self;
}

# Add -/ipc and -/ctcmd to list of supported attr-flags.
sub attropts {
    my $self = shift;
    return $self->SUPER::attropts(@_, qw(ipc ctcmd));
}

sub _chdir {
    my $self = shift; # this might be an instance or a classname
    my ($dir) = @_;
    chomp $dir;
    my $rc = CORE::chdir($dir);
    if ($self->ipc) {
        my $ct = ref($self) ? $self->clone : __PACKAGE__->new;
        $ct->stdout(0)->argv('cd', $dir)->system;
    }
    return $rc;
}

# Export our own functional interfaces as well.
sub ctsystem	{ return __PACKAGE__->new(@_)->system }
sub ctexec	{ return __PACKAGE__->new(@_)->exec }
sub ctqx	{ return __PACKAGE__->new(@_)->qx }
sub ctpipe	{ return __PACKAGE__->new(@_)->pipe }
sub chdir	{ return __PACKAGE__->_chdir(@_) }
*ctqv = \&ctqx;  # just for consistency

# Constructor, and copy constructor as clone, with $proto
sub new {
    my $proto = shift;
    my $self = $proto->SUPER::new(@_);
    if ($self->ipc) {
	++$pidcount{$self->{IPC}->{PID}};
	if (ref($proto)) {
	    # Correct the effect of the base cloning on globs
	    $self->{IPC}->{DOWN} = $proto->{IPC}->{DOWN};
	    $self->{IPC}->{BACK} = $proto->{IPC}->{BACK};
	}
    }
    bless $self, ref($proto) ? ref($proto) : $proto;
    return $self;
}
*clone = \&new;

# Clean up leftover cleartool processes if user forgot to.
sub DESTROY {
    my $self = shift;
    $self->ipc(0) if $self->ipc;
}

1;

__END__

=head1 NAME

ClearCase::Argv - ClearCase-specific subclass of Argv

=head1 SYNOPSIS

    # OO interface
    use ClearCase::Argv;
    ClearCase::Argv->dbglevel(1);
    # Note how the command, flags, and arguments are separated ...
    my $describe = ClearCase::Argv->new('desc', [qw(-fmt %c)], ".");
    # Run the basic "ct describe" command.
    $describe->system;
    # Run it with with stderr turned off.
    $describe->stderr(0)->system;
    # Run it without the flags.
    $describe->system('-');
    # Run it through a pipe.
    $describe->pipecb(sub { print shift; return 1; });
    $describe->pipe;
    # Create label type XX iff it doesn't exist
    ClearCase::Argv->new(qw(mklbtype -nc XX))
	    if ClearCase::Argv->new(qw(lstype lbtype:XX))->stderr(0)->qx;

    # Functional interface
    use ClearCase::Argv qw(ctsystem ctexec ctqx ctpipe);
    ctsystem('pwv');
    my @lsco = ctqx(qw(lsco -avobs -s));
    # Similar to OO example: create label type XX iff it doesn't exist
    ctsystem(qw(mklbtype XX)) if !ctqx({stderr=>0}, "lstype lbtype:XX");
    ClearCase::Argv->pipecb(sub { print "GOT: " . shift() . "\n"; 1 });
    ctpipe({autochomp => 1},'lsview', ['-l']);

I<There are more examples in the ./examples subdir> that comes with this
module. Also, I<the test script is designed as a demo and benchmark> and
is a good source for cut-and-paste code.

=head1 DESCRIPTION

I<ClearCase::Argv> is a subclass of I<Argv> for use with ClearCase.  It
exists to provide an abstraction layer over the I<cleartool>
command-line interface. A program written to this API can be told to
send commands to ClearCase via the standard technique of executing
cleartool or via the ClearCase::CtCmd module or via a pipe to cleartool
(aka C<IPC mode>) by flipping a switch.

To that end it provides a couple of special methods I<C<ctcmd>> and
I<C<ipc>>. The C<ctcmd> method can be used to cause cleartool commands
to be run in the current process space using I<ClearCase::CtCmd>.
Similarly, C<ipc> will send commands to a cleartool co-process.  See
the documentation of these modules for details on what they do, and see
I<ALTERNATE EXECUTION INTERFACES> below for how to invoke them. Sample
scripts are packaged with I<ClearCase::Argv> in ./examples.

I<As ClearCase::Argv is in most other ways identical to its base
class>, see C<perldoc Argv> for substantial further documentation.>

=head2 OVERRIDDEN METHODS

A few methods of the base class I<Argv> are overridden with modified
semantics. These include:

=over 4

=item * prog

I<ClearCase::Argv-E<gt>prog> prepends the word C<cleartool> to each
command line when in standard (not C<-E<gt>ctcmd> or C<-E<gt>ipc>)
mode.

=item * quote

The cleartool "shell" has its own quoting rules. Therefore, when using
C<-E<gt>ctcmd> or C<-E<gt>ipc> modes, command-line quoting must be
adjusted to fit cleartool's rules rather than those of the native
system shell, so the C<-E<gt>quote> method is extended to handle that
case.

=item * readonly

It's sometimes useful to set the following class attribute:

    ClearCase::Argv->readonly('auto');

This does nothing by itself but it modifies the behavior of the
I<-E<gt>noexec> attribute: instead of skipping execution of all
commands, it only skips commands which modify ClearCase state.

Consider a script which does an C<lsview> to see if view XYZ exists
followed by a C<mkview> to create it if not, and has a I<-n> flag to
say C<show what you would do without doing it>, implemented internally
by setting I<-E<gt>noexec>. Without this setting it wouldn't even do
the lsview so you can't find out if it would do the mkview.  With it,
however, the lsview would be performed while the mkview would be shown
but skipped as intended.  Running C<read> commands while skipping
C<write> commands causes scripts to behave far more realistically in
I<-E<gt>noexec> mode.

=item * outpathnorm

On Windows, cleartool's way of handling pathnames is underdocumented,
complex, and arguably broken. Apparently, given a choice, cleartool on
Windows always prefers and uses the native (\-separated) format. Though
it will understand and (mostly) preserve /-separated pathnames, any
path information it I<adds> (notably version-extended data) is
B<always> \-separated. For example:

    cleartool ls -s -d x:/vobs_xyz/foo/bar

will return something like

    x:/vobs_xyz/foo\bar@@\main\3

Note that the early forward slashes are retained but the last / before
the C<@@> becomes a \, while all version info after the C<@@> uses \.
It looks like CC splits the path into dirname and basename, does
whatever it's asked to do, then pastes the path back together using the
native separator character before printing it.

Normalizing pathnames is difficult because there's no way to determine
with certainty which lines in the output of a cleartool command are
pathnames and which might just happen to look like one. I.e. the phrase
"either/or" might occur in a comment returned by I<cleartool describe>;
should we interpret it as a pathname?

The strategy taken by the I<Argv-E<gt>outpathnorm> attribute of the
base class is to "fix" each line of output returned by the I<-E<gt>qx>
method B<iff> the I<entire line>, when considered as a pathname, refers
to an existing file.  This can miss pathnames which are not alone on a
line, as well as version-extended pathnames within a snapshot view.

Having the advantage of knowing about ClearCase, the overridden
I<ClearCase::Argv-E<gt>outpathnorm> extends the above strategy to also
modify any strings internal to the line which (a) look like pathnames
and (b) contain C<@@>. This errs on the side of caution: it will rarely
convert strings in error but may not convert pathnames in formats where
they are neither alone on the line nor contain version-extended info.
It can also be foiled by pathnames containing whitespace or by a change
in the extended naming symbol from C<@@>.

In summary, I<ClearCase::Argv-E<gt>outpathnorm> will normalize (a) all
version-extended pathnames and (b) paths of any type which are alone on
a line and refer to an existing filesystem object.

=back

=head2 ADDITIONAL METHODS

These are methods not offered by I<Argv>.

=over 4

=item * comment

Any text passed to the C<-E<gt>comment> method will be provided to the
next cleartool command as a comment. E.g.:

    ClearCase::Argv->ci('foo')->comment("Multi-line\nComment")->system;

This is useful because it takes care of the quoting and other
machinations necessary to deal with the different execution methods.
For instance in IPC mode the comment will be fed to cleartool on stdin
whereas in exec mode it will be passsed on the command line.  When
supplying text via the comment method it is your responsibility to
ensure that the very next command is one which takes a standard
ClearCase comment. You may also want to turn off stdout for the same
command in order to suppress the comment prompt.

=back

=head1 ALTERNATE EXECUTION INTERFACES

The I<C<-E<gt>ctcmd>> method allows you to send cleartool commands
directly to ClearCase via the CtCmd interface rather than by exec-ing
cleartool itself.

When called with no argument it returns a boolean indicating whether
I<CtCmd mode> is on or off. When called with a numerical argument, it
sets the CtCmd mode as follows: if the argument is 0, CtCmd mode is
turned off and subsequent commands are sent to real cleartool via the
standard execution interface.  With an argument of 1, it attempts to
use CtCmd mode but if CtCmd fails to load for any reason it will
(silently) run commands via CAL instead.  With an argument of 2 the
behavior is the same but a warning ("CtCmd not found - using CAL
instead") is printed.  With an argument of 3 the warning becomes a
fatal error, thus using CtCmd I<only> if the compiled version is
installed.

=head2 Examples

    # Use CtCmd if available, else continue silently using CAL
    ClearCase::Argv->ctcmd(1);
    # Use CtCmd if available, else print warning and use CAL
    ClearCase::Argv->ctcmd(2);
    # Use CtCmd if available, else die with error msg
    ClearCase::Argv->ctcmd(3);
    # Turn off use of CtCmd
    ClearCase::Argv->ctcmd(0);

Typically C<-E<gt>ctcmd> will be used as a class method to specify a
place for all cleartool commands to be sent. However, it may also be
invoked on an object to associate just that instance with CtCmd.

A similar sequence is observed for C<-E<gt>ipc mode> except for the
different method name, e.g.:

    # Use IPC if available, else abort
    ClearCase::Argv->ipc(3);

Note: you can tell which mode is in use by turning on the I<dbglevel>
attribute. Verbosity styles are as follows:

    + cleartool pwv		# standard (fork/exec)
    +> pwv			# CtCmd
    => pwv			# IPC

CtCmd mode is not compatible with the I<-E<gt>ctpipe> method.
Therefore, when a pipe is requested it will result in a new process
created by the traditional execution interface.

A final note on IPC and CtCmd modes: turning on one will automatically,
and silently, turn off the other. I.e. the sequence

    ClearCase::Argv->ipc(2);
    ClearCase::Argv->ctcmd(2);

will not throw any exceptions and will leave you in CtCmd mode. The
coprocess will be shut down.

=head1 FUNCTIONAL INTERFACE

For those who don't like OO style, or who want to convert existing
scripts with the least effort, the I<execution methods> are made
available as traditional functions. Examples:

	use ClearCase::Argv qw(ctsystem ctexec ctqx ctpipe);
	my $cwv = ctqx(pwv -s);
	ctsystem('mklbtype', ['-global'], 'FOO') && exit $? >> 8;
	my @vobs = ctqx({autochomp=>1}, 'lsvob -s');
	ctpipe('lsview', ['-l'], sub { print "GOT: " . shift() . "\n"; 1 });

These interfaces may also be imported via the I<:functional> tag:

	use ClearCase::Argv ':functional';

=head1 CAREFUL PROGRAMMERS WANTED

If you're the kind of programmer who tends to execute whole strings
such as C<system("cleartool pwv -s")> reflexively or who uses
backquotes in a void context, this module won't help you much because
it can't easily support those styles. These are deprecated techniques
regardless of whether you use ClearCase::Argv and you should strive to
overcome them.

=head1 STICKINESS

A subtlety: when an execution attribute is set in a void context, it's
I<"sticky">, meaning that it's set until explicitly reset. But in a
non-void context the new value is temporary or I<"non-sticky">; it's
pushed on a stack and popped off after being read once. This applies to
both class and instance uses. It's done this way to allow the following
locutions:

    ClearCase::Argv->stdout(0);	# turn off stdout for all objects
    $obj1->stdout(0);		# turn off stdout for this object, forever
    $obj2->stdout(0)->system;	# suppress stdout, this time only

This allows you to set up an object with various sticky attributes and
keep it around, executing it at will and overriding other attrs
temporarily. In the example below, note that another way of setting
sticky attrs is illustrated:

    my $obj = ClearCase::Argv->new({autofail=>1, autochomp=>1});
    my $view = $obj->argv('pwv -s')->qx;
    my $exists = $obj->argv('lstype', 'brtype:FOO')->autofail(0)->qx;

Here we keep an object with attrs 'autochomp' and 'autofail' (autofail
means to exit on any failure) around and use it to exec whichever
commands we want. While checking to see if a type exists, we suppress
autofail temporarily. On the next use the object will have both
attributes again.

=head1 BUGS

I suspect there are still some special quoting situations unaccounted
for in the I<quote> method. This will need to be refined over time. Bug
reports or patches gratefully accepted.

Commands using a format option defining a multiline output fail in many
cases in fork mode, because of the underlying Argv module.

ClearCase::Argv will use IPC::ChildSafe if it finds it, which may 
introduce differences of behavior with the newer code to replace it.
It should probably just drop it, unless explicitly driven to use it.

Autochomp should be equivalent in all modes on all platforms, which
is hard to test (ipc w/wo IPC::ChildSafe, ctcmd, on Unix and Windows,
with system and qx...).
The autochomp setting should not affect system calls in ipc mode!?
Hopefully it doesn't anymore. Problem: change not satisfactorily tested
on Windows yet (where the output prior to the last change seemed ok...)

Argv uses the first of three different methods for cloning, and I
(Marc Girod) suspect that only the first one (Clone, in recent versions)
performs correctly with GLOB objects... Work-around in place.

The string 'cleartool' is hard-coded in many places, making it hard to
implement additional support for multitool commands.

The use of 'cleartool -status' needed to be disabled, and simulated,
because of a ClearCase bug, with setview exiting the interactive session
to the shell (Found from 2002.05.00 to 7.0.1).
The result in an ipc mode using '-status' was: hang.

Cygwin is not supported on Windows.

=head1 PORTABILITY

ClearCase::Argv should work on all supported ClearCase platforms and
versions. It's currently maintained on Solaris 9 and Windows XP with CC
7.0 using Perl5.8.x.  Viability on other platforms and/or earlier
versions is untestable by me.

Marc Girod's testing environment: Solaris 8 and 10, and Windows 2000, 
without CtCmd and IPC::ChildSafe, with Clone.
Tatyana Shpichko's testing environment: RedHat Linux 4, with and without
CtCmd, and Windows XP without CtCmd.

=head1 FILES

This is a subclass of I<Argv> and thus requires I<Argv> to be
installed.  ClearCase::CtCmd is required for I<ctcmd mode> in Unix.
In Windows, I<Win32-Process-Info> or I<Win32::ToolHelp> is required
for I<pipe> support.

=head1 SEE ALSO

perl(1), Argv, ClearCase::CtCmd

=head1 AUTHOR

David Boyce <dsbperl AT boyski.com>

=head1 COPYRIGHT

Copyright (c) 1999-2007 David Boyce. All rights reserved.  This Perl
program is free software; you may redistribute it and/or modify it
under the same terms as Perl itself.

=head1 GUARANTEE

Double your money back!

=cut
