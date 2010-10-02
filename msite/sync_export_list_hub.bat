@rem = ' This is a perl script for NT and UNIX.
@echo off
goto endofperl
@rem ';
# -*- tab-width: 4 -*-

#---------------------------------------------------------------------------+
# Licensed Materials - Property of IBM
# Rational ClearCase
# Copyright IBM Corp. 1998, 2006 All Rights Reserved
# US Government Users Restricted Rights -
# Use, duplication or disclosure restricted by GSA ADP Schedule Contract
# with IBM Corp.
#---------------------------------------------------------------------------+

###########################################################################
# +-------------------------------------------------------------------------+
# Minor modifications for use with a hub.
# The script was renamed from the original sync_export_list.bat, in order to
# avoid its being overwritten in a ClearCase update
#---------------------------------------------------------------------------+
# sync_export_list_hub.bat
# usage: sync_export_list_hub - run with "-help" for detailed usage.
#
# This script is intended to be run periodically by the ClearCase
# Job Scheduler to send synchronization update packets to 
# other replica(s) of one or more vob(s).
#
# It may also be used to initiate sync export activity ad-hoc from the
# command line with a variety of options.
# 
# Run the script with the "-help" switch for description of the parameters.
###########################################################################

use strict;
use Getopt::Long;

sub dbgprint;    # predeclare it; routine in MScommon.pl
sub qprint;      # predeclare...

my $thisscript = $0;
if ($thisscript =~ m/(.*)(\.bat)$/) {
    $thisscript = $1;
}
my $pkg_name = 'sync_export_list_hub';
$CFG::verbose = ($ENV{TRACE_SUBSYS} && ($ENV{TRACE_SUBSYS} =~ m/$pkg_name/));
$CFG::failcount = 0;
$CFG::quietmode = 0;

# force debugging msgs by uncommenting the following.....
# $CFG::verbose = 1;

# figure out which architecture we're on. The Config.pm package and $^O Perl variable
# both have changed depending on the version of perl or ccperl that actually runs this
# script, so we need to try a couple of combinations.

use Config;
my $arch = $^O ? $^O : $main::Config{archname};
$CFG::NT = (($arch =~ /^MSWin32/) || ($arch eq 'i386-win32')) ? 1 : 0;
$CFG::UNIX = ($CFG::NT ? 0 : 1);

# CLEARCASEHOME e.v. should always be set on NT when called by ccperlpp.pl,
# so exit if it's not set. On UNIX, assume /opt/rational/clearcase if user
# hasn't explicitly set CLEARCASEHOME (or deprecated ATRIAHOME).
#
if ($CFG::UNIX) {
    if (!exists $ENV{CLEARCASEHOME}) {
        # check for (now deprecated) ATRIAHOME e.v.; use if set.
        if (exists $ENV{ATRIAHOME} ) {
            $ENV{CLEARCASEHOME} = $ENV{ATRIAHOME};
        } else {
            $ENV{CLEARCASEHOME} = "/opt/rational/clearcase";
        }
    }
    my $producthome = $ENV{CLEARCASEHOME};

    chdir $producthome;
    push(@INC, "$producthome/config/scheduler/tasks");
} else {
    my $producthome = $ENV{CLEARCASEHOME} or 
                      die "CLEARCASEHOME environment variable not set\n";
    chdir $producthome;
    push(@INC, "$producthome\\config\\scheduler\\tasks");
    $thisscript = $pkg_name;
}

# set script version to test for mismatch against the MScommon.pl version.
$CFG::MS_script_version = "7.0.0.0";

require 'MScommon.pl';
require 5.8.6;

if (!$CFG::MScommon_version) {
    $CFG::MScommon_version = "(unknown older version)";
}

# check for mismatched versions - MScommon.pl should set
# $CFG::MScommon_version; if not set or unequal, may indicate that
# an older/newer version is being included improperly.

if ($CFG::MScommon_version ne $CFG::MS_script_version) {
    die "Version mismatch between require file 'MScommon.pl' " .
        "(version: ${CFG::MScommon_version}) and \n" .
        "the MultiSite export script which includes it, " .
        "'sync_export_list_hub.bat' " .
        "(version: ${CFG::MS_script_version}).\n" . 
        "Ensure that old versions of the script/require file are not in use.\n";
}

$CFG::failcount = 0;

$| = 1 if $CFG::verbose;

dbgprint "running on architecture $arch\n";

my $workingdir_cmd = ($CFG::NT ? 'cd' : 'pwd');

###########################################################################
# main
#
# get command line options figured out...
dbgprint "ARGV: " . join(' ',@ARGV) . "\n";
my $do_compress = 0;
my $export_vob = '';
my $logdir = '';
my $st;
my $help = 0;
my $sclass = '';
my $reps = '';
my @replicas;
my $all_local = 0;
my $maxpacket = '';
my $fshipflg = 1;
my $shipflg = 0;
my $lockwait = 0;
$CFG::traceflg = 0;
my $updateflg = 0;
my $limit = 0;
my $poll = 0;
my $num_tries = 1;  # default: only one try
my $sleeptime = 30;
$CFG::workdir = '';
my $usage = 
    "$thisscript [-[no]c] [-lo logdir] [-h] [-f | -sh] [-lockwait minutes]\n" .
    "    [-q mode] [-wo workdir] [-m maxsize[k|m|g]]\n" .
    "    [-sc sclass] [-u] [-li limit] [-t] [-[no]p] [-i tries] [-wa wait]\n" .
    "    { -all | -r replica-list [script-file] | script-file }\n" .
    "    More help is available by using the '-help' command-line switch.\n";
my $helpmsg = <<ENDHELP;
=========================================================================
"sync_export_list" exports one or more VOB replicas. The VOBs/replicas to
be exported, as well as options for the operation, may be specified either
on the command line, or via a script-file whose name is specified as a
parameter, or a combination of these two methods.

A logfile is created during execution, but is normally deleted if the script 
executes successfully, and is kept if a failure is detected.

Setting the environment variable TRACE_SUBSYS to "sync_export_list" will turn
on all debugging print statements (verbose mode). Using the "-trace" command
line switch will print a less-verbose set of messages. Using either tracing
method will retain the logfile even when the script is successful.

NOTE:
This script utilizes new features of ClearCase/MultiSite 4.0; some options
may not work correctly with earlier versions of ClearCase.

$usage
Command line options:

    -[no]compress       Turns compression of exported packets on/off.
                        (Default is "-nocompress").

    -all                Export from all local replicas. Other "-replicas"
                        parameters on command line and any scriptfile specified
                        will be ignored if '-all' is specified.

    -logdir directory   Specify directory to write log file. The log file will
                        be given a unique name in this directory based
                        on the process ID and timestamp. Directory must be
                        writeable by the user.
                        Defaults:
                        UNIX: /var/adm/rational/clearcase/log/sync_logs
                        NT:   {CLEARCASEHOME}\\var\\log

    -workdir directory  Forces script to use this directory as its temporary
                        directory. Directory must exist and be writeable.
                        Default: script uses GetTmpdir() to find a temporary
                        directory to use.

    -help               Print this message and exit.

    -fship              Force shipping of packets immediately (default).

    -ship               Overrides -fship (packets will wait until shipping
                        server runs).

    -maxsize size       Max packet size, with number followed by size letter,
                        e.g. "500k", "20m", "1.5g". If not specified, MultiSite
                        will use the default size from shipping.conf (UNIX)
                        or the Control Panel applet (NT).

    -limit max-packets  Maximum number of packets to generate. (Default:
                        no limit).

    -sclass class       Storage class to use. (Default: use default sclass).

    -[no]trace          Display commands as they are executed; and
                        lists command-line options passed in.
                        (Default: "-notrace").

    -[no]update         Update this replica's epoch table from remote replica
                        before performing export. (Default: "-noupdate").

    -[no]poll           Perform "shipping_server -poll" before exporting.
                        Uses "-sclass" if specified. (Default: "-nopoll").

    -lockwait minutes   The amount of time (in minutes) to wait on VOB 
			locks that block VOB writes. Minutes must be > 0.
			(Default: no waiting).
			NOTE: If VOB locked and lockwait NOT set, old
			values in epoch table may cause unnecessary packet
			transfers on subsequent exports.

    -replicas repnames  Specify a VOB and replica(s) to use. You may specify
                        a single replica: rep1@\\myvob, or multiple replicas
                        as: rep1@\\myvob,rep2,rep3. If you specify only the
                        VOB as: \\myvob, syncs will be generated for ALL
                        of this replica's siblings. If you wish to use more
                        than one VOB, use a 'script-file' as described below.
                        NOTE: if you specify multiple replicas, you must not 
                        leave any white space in the parameter.

    -iterate num_tries  Maximum number of tries to get all exports to complete
                        successfully. A retry may be successful for various
                        reasons, such as a lock being released or a VOB
                        mounted. (Default: 1, indicating that failed exports
                        will NOT be retried).

    -wait seconds       Useful if '-iterate' is specified. The amount of wait
                        time (in seconds) before re-attempting failed exports.
                        (Default: 30 seconds).

    -quiet mode         Quiet mode = 1 will suppress all non-ERROR/WARNING
                        messages normally sent to STDOUT. If set to 2,
                        all STDOUT messages are suppressed.
                        (Default: 0 - prints various informational messages).

    script-file         Path to file containing a set of directives to 
                        execute. Must be the final parameter specified on
                        the command line. (Default is no script-file).

                        Script-file may contain the following directives;
                        directives with values will have the value immediately
                        following a colon (":"). All directives are processed
                        in sequence, and those which affect export settings
                        (i.e. all except 'replicas') remain in effect for all
                        subsequent 'replicas' directives unless modified.
                        
                        [no]compress - set/unset export compression.
                        maxsize:<size-specifier> - set max packet size.
                        ship -  export with "-ship" qualifier.
                        fship - export with "-fship" qualifier.
                        limit:<number> - set max number of packets
                            to export per replica.
                        [no]update - set/unset whether epoch table is updated
                            prior to export.
                        sclass:<storage-class> - set a different storage class;
                            leave value blank to unset.
                        replicas:<list> - initiates the export for list of
                            VOB replicas. See above for format of list.
                        lockwait:<number> - set minutes to wait for VOB locks

Several features may be configured for the MultiSite import and export scripts by means
of a script configuration file, located at '${CFG::script_configfile}'.
To affect the export script, the config file may include the following line:

disable_export_locking = 1

which will disable use of the export lockfile, and allow multiple exports to run
on a single replica simultaneously. Leaving this at a value of '0' (default) will
leave export locking enabled. Other parameters affect only the import script.
=========================================================================
ENDHELP

$st = GetOptions("compress!" => \$do_compress,
                 "logdir=s" => \$logdir,
                 "help" => \$help,
                 "all" => \$all_local,
                 "fship" => \$fshipflg,
                 "ship" => \$shipflg,
                 "lockwait=i" => \$lockwait,
                 "trace!" => \$CFG::traceflg,
                 "maxsize=s" => \$maxpacket,
                 "limit=i" => \$limit,
                 "poll!" => \$poll,
                 "update!" => \$updateflg,
                 "quiet=i" => \$CFG::quietmode,
                 "replicas=s" => \$reps,
                 "wait=i"     => \$sleeptime,
                 "workdir=s"   => \$CFG::workdir,
                 "iterate=i"  => \$num_tries,
                 "sclass=s" => \$sclass);

if ($help) {
    qprint "$helpmsg\n";
    exit 1;
}

if (scalar @ARGV > 1) {
    qprint "ERROR: too many script files specified in cmd line!\n";
    exit 1;
}

$CFG::tmpdir = GetTmpdir();

dbgprint "temporary dir is: $CFG::tmpdir\n";

if ($CFG::workdir) {
    if (! -w $CFG::workdir) {
        # the temp directory we're supposed to use is not writeable.
        print STDERR "Working directory specified '$CFG::workdir' is not writeable, using '$CFG::tmpdir'\n";
        qprint "WARNING: Working directory '$CFG::workdir' is not writeable. Continuing.\n";
        $CFG::workdir = '';
    } else {
        $CFG::tmpdir = $CFG::workdir;
        if ($CFG::UNIX) {
            $ENV{TMPDIR} = $CFG::tmpdir;
        } else {
            $ENV{TMP} = $CFG::tmpdir;
        }
    }
}    

if (!$st) {  # bad status or more than one arg left
    qprint "ERROR: error getting command line options.\nUsage:\n$usage\n";
    exit 1;
}

if ($shipflg) {  # "-ship" will override "-fship"
    $fshipflg = 0;
}

if ($lockwait < 0) {  # "-lockwait" value must be > 0
    qprint "ERROR: -lockwait value must be > 0.\n";
    exit 1;
}

if ($lockwait > 0 ) {  # "-lockwait" causes EV to be set
    $ENV{"CLEARCASE_VOBLOCKWAIT"} = "$lockwait";
} else {	       # otherwise, delete EV to be sure not inheriting
    delete $ENV{"CLEARCASE_VOBLOCKWAIT"};
}


my $scriptfile = $ARGV[0] if (scalar @ARGV);  # script/config file specified

print "command options specified or defaulted:\n    compress: $do_compress\n" .
    "    logdir: $logdir\n    storage-class: $sclass\n    workdir: $CFG::workdir\n" .
    "    maxpacket: $maxpacket\n    limit: $limit\n    all: $all_local\n" .
    "    fship: $fshipflg\n    ship: $shipflg\n    poll: $poll\n" .
    "    lockwait: $lockwait minutes\n" .
    "    retries: $num_tries times, wait $sleeptime seconds\n" .
    "    script: $scriptfile\n" if ($CFG::verbose || $CFG::traceflg);

#
# Validate that the log directory actually exists...
#
$logdir = ValidateLogDir($logdir);

#
# Redirect STDERR to a uniquely named log file
#
my $logname = RedirectSTDERRtoLog ($logdir, 'send');

print STDERR "\n============================================================\n" .
    "Beginning execution of MultiSite export script '$thisscript' at " .
    MakeTimestamp() . ".\n\n";

if ($reps) {
    ($export_vob,@replicas) = GetReplicas($reps);
    if (!$export_vob) {
        qprint "ERROR: error parsing '-replicas' argument.\n$usage\n";
        exit 1;
    }
}
print "    vob: $export_vob\n    replicas: " . join(" ",@replicas) . "\n"
    if ($CFG::verbose || $CFG::traceflg);

# begin by forcing any existing packets to ship if "-poll" specified on cmd
# line
if ($poll) {
    my $sclass_expr = ($sclass ? "-sclass $sclass" : "");
    my $cmd = "$CFG::shipping_server -poll $sclass_expr";
    my $res = Do_System($cmd, __FILE__, __LINE__);
    if ($res) {
        print STDERR "ERROR: error executing \"$cmd\" - continuing.\n";
        $CFG::failcount++;
    }
}

# use this array to collect names of all reps to export, along with
# params for that particular export. After parsing all cmd line and
# script-file stuff, then go ahead and start sending packets.
my @PktsToSend;
my $NumPkts = 0;

# if "-all" specified, do all local replicas and force to ignore '-replicas' and scriptfile
if ($all_local) {
    my @vobs = GetLocalVobs();
    dbgprint "localvobs: " . join(" ", @vobs) . "\n";
    my $vob;
    foreach $vob (@vobs) {
        my @siblings = GetSiblings( $vob );
	next unless scalar @siblings;
	my $rep =
	    DoCmd("$CFG::ClearTool des -fmt '%[replica_name]p' vob:$vob");
	my %locepo = GetReplicaEpochs($vob, $rep);
        my $sib;
        foreach $sib (@siblings) {
	    my %remepo = GetReplicaEpochs($vob, $sib);
	    my $skip = 1;
	    my $key;
	    foreach $key (keys %remepo) {
	        if ($remepo{$key} < $locepo{$key}) {
	            $skip = 0;
	            last;
	        }
	    }
	    next if $skip;
	    $PktsToSend[$NumPkts++] = CollectPacket($vob, $sib, $do_compress,
                $maxpacket, $limit, $sclass, $fshipflg, $updateflg, $lockwait);
        }
    }
# ensure we ignore -replicas or scriptfile by clearing any remaining work items
@replicas = ();
$scriptfile = '';
}

# sync any replicas specified on cmd line
my $rep;
foreach $rep (@replicas) {
    if ($export_vob) {
        $PktsToSend[$NumPkts++] = CollectPacket($export_vob,
                                                $rep, 
                                                $do_compress, 
                                                $maxpacket,
                                                $limit,
                                                $sclass,
                                                $fshipflg,
                                                $updateflg,
                                                $lockwait);
    } else {
        qprint "ERROR: no VOB specified for replica \"$rep\".\n";
        print STDERR "ERROR: no VOB specified for replica \"$rep\".\n";
        $CFG::failcount++;
    }
}

if ($scriptfile && !(-e $scriptfile)) {
    print STDERR "ERROR: Script file $scriptfile does not exist\n";
    qprint "ERROR: Script file $scriptfile does not exist\n";
    $scriptfile = '';
    $CFG::failcount++;
}

if ($scriptfile) {
    my ($tag, $val);
    dbgprint "opening export script '$scriptfile'.\n";
    open (SCR, "<$scriptfile");
    while (<SCR>) {
        chomp;
        ($tag, $val) = split(":", $_);
        dbgprint "scriptfile - tag: $tag value: $val\n";
        @replicas = ();
        if ("replicas" =~ m/^$tag/) {
            dbgprint "found replicas...\n";
            ($export_vob,@replicas) = GetReplicas($val);
        } elsif ("maxsize" =~ m/^$tag/) {
            dbgprint "found maxsize...\n";
            $maxpacket = $val;
        } elsif ("sclass" =~ m/^$tag/) {
            dbgprint "found sclass...\n";
            $sclass = $val;
        } elsif ("nocompress" =~ m/^$tag/) {
            dbgprint "found nocompress...\n";
            $do_compress = 0;
        } elsif ("compress" =~ m/^$tag/) {
            dbgprint "found compress...\n";
            $do_compress = 1;
        } elsif ("update" =~ m/^$tag/) {
            dbgprint "found update...\n";
            $updateflg = 1;
        } elsif ("noupdate" =~ m/^$tag/) {
            dbgprint "found noupdate...\n";
            $updateflg = 0;
        } elsif ("ship" =~ m/^$tag/) {
            dbgprint "found ship...\n";
            $fshipflg = 0;
            $shipflg = 1;
        } elsif ("limit" =~ m/^$tag/) {
            dbgprint "found limit...\n";
            $limit = $val;
        } elsif ("fship" =~ m/^$tag/) {
            dbgprint "found fship...\n";
            $fshipflg = 1;
            $shipflg = 0;
        } elsif ("lockwait" =~ m/^$tag/) {
            dbgprint "found lockwait...\n";
            $lockwait = $val;
        } else {
            print STDERR "ERROR: unrecognized option '$tag' in scriptfile $scriptfile\n";
            qprint "ERROR: unrecognized option '$tag' in scriptfile $scriptfile\n";
            $CFG::failcount++;
        }
        #
        # if we found a 'replicas' tag in the file, start export processing
        #
	foreach $rep (@replicas) {
	    if ($export_vob) {
		$PktsToSend[$NumPkts++] = CollectPacket($export_vob, $rep, 
						$do_compress, $maxpacket, 
						$limit, $sclass, $fshipflg, 
						$updateflg, $lockwait);
	    } else {
		print "No sync done; no VOB specified for replica \"$rep\".\n";
	    }
	}
    }
}

print STDERR "using options:\n    compress: $do_compress\n" .
    "    logdir: $logdir\n    storage-class: $sclass\n    workdir: $CFG::workdir\n" .
    "    maxpacket: $maxpacket\n    limit: $limit\n    all: $all_local\n" .
    "    fship: $fshipflg\n    ship: $shipflg\n    poll: $poll\n" .
    "    lockwait: $lockwait minutes\n" .
    "    retries: $num_tries times, wait $sleeptime seconds\n" .
    "    script: $scriptfile\n" if ($CFG::verbose || $CFG::traceflg);

dbgprint "\@PktsToSend:\n";
dbgprint "@PktsToSend";
dbgprint "\n";

my %pts = ();
foreach (@PktsToSend) {
  my ($vob, $rep, $opt)
    = split(";", $_, 3);
  push @{$pts{$opt}{$vob}}, $rep;
}

my $pkts_left = 0;
while ($num_tries--) {
    # this loop takes every packet for the same vob (list of replicas with
    # a common set of options), from the %pts hash and attempts to send it.
    # If successful, it removes the packet from the hash, which in the end,
    # contains only the packets that were NOT successfully processed.
    $pkts_left = 0;
    my ($o, $v);
    for $o (keys %pts) {
	for $v (keys %{$pts{$o}}) {
	    my ($do_compress, $maxpacket, $limit, $sclass, $fshipflg, $updateflg, $lockwait)
		= split(";", $o);
	    my @rep = @{$pts{$o}{$v}};
	    dbgprint "send pkt parms: $v @rep $do_compress $maxpacket $limit $sclass $fshipflg $updateflg $lockwait\n";
	    my $res = SendPacket($v, $do_compress, $maxpacket, $limit,
				 $sclass, $fshipflg, $updateflg, $lockwait, @rep);
	    if ($res) {
		$CFG::failcount++;
	    } else {
		delete $pts{$o}{$v};
	    }
	}
	$pkts_left += scalar keys %{$pts{$o}};
    }
    
    dbgprint "packets left: $pkts_left; tries left: $num_tries\n";
    if ($num_tries && $pkts_left) {
	qprint "$pkts_left packets not exported; waiting $sleeptime seconds to re-try.\n";
	print STDERR "$pkts_left packets not exported; waiting $sleeptime seconds to re-try.\n";
	sleep $sleeptime;
    }
}

my ($cmd, $res);
if ($CFG::failcount && $pkts_left) {
    if ($CFG::NT) { # only do event log for NT
	$cmd = "$CFG::imsglog $pkg_name evtlog \"$logname\"";
	$res = Do_System($cmd, __FILE__, __LINE__);
    }
    qprint "\n\n\nERROR: $CFG::failcount error(s) encountered during export processing.\n$pkts_left packet(s) may not have been sent successfully. See log at:\n    \"$logname\".\n\n\n";
    print STDERR "\nERROR: $CFG::failcount error(s) encountered during export processing.\n$pkts_left packet(s) may not have been sent successfully.\n";
    close STDERR;
} else {
    if ($CFG::NT) { # only do event log for NT
	$cmd = "$CFG::imsglog $pkg_name evtlog";
	$res = Do_System($cmd, __FILE__, __LINE__);
    }
    if ($CFG::failcount) {
        # there's an error count, but no packets are left. The errors must
        # have been only temporary. Send out a msg to explain this.
        my $msg = "\nINFO: $CFG::failcount temporary errors were " .
            "encountered, but all\n    packets were successfully exported.\n";
        qprint $msg;
        print STDERR $msg;
    }
    if ($CFG::verbose || $CFG::traceflg || $CFG::failcount) {  # keep log file if set
        qprint "\n\nSUCCESSFUL COMPLETION: see log file at:\n    \"$logname\".\n\n";
        print STDERR "\nSUCCESSFUL COMPLETION of export processing.\n";
        close STDERR;
    } else {
        qprint "\n\nSUCCESSFUL COMPLETION: log file removed.\n\n";
        close STDERR;
        unlink $logname;
    }
}

close STDOUT;
exit $CFG::failcount;

sub CollectPacket {
    # concat all SendPacket() params into single entity to store in array
    return join(";", @_);
}

sub SendPacket {
    #
    # Send the replica synchronization packet
    #

    my ($export_vob, $do_compress, $maxpacket, $limit, $sclass, $fshipflg,
	$updateflg, $lockwait, @replicas) = @_;

    my $failcount = 0;
    my ($cmd, $res);
    my ($packet,$packet_qq);
    my $rep_sel = join ' ', map { sprintf "replica:$_\@$export_vob", $_ } @replicas;
    if ($lockwait < 0) {  # "lockwait" value must be > 0
        qprint "ERROR: lockwait value must be > 0.\n";
        exit 1;
    }
    if ($lockwait > 0) { # "-lockwait" causes EV to be set - value must be > 0
        $ENV{"CLEARCASE_VOBLOCKWAIT"} = "$lockwait";
    } else {	     # otherwise, delete EV to be sure not inheriting
        delete $ENV{"CLEARCASE_VOBLOCKWAIT"};
    }

    # use MS script config file setting to turn off export locking if requested.
    $CFG::disable_locking = $CFG::MScfg_disable_export_locking;
    
    # attempt to get advisory lock based on first tag/replica we're exporting to.
    # return error if we can't get the lock.
    my $dense_rep_oid = GetDenseOid((split / /, $rep_sel)[0]);
    if ($dense_rep_oid !~ m/([0-9][a-f])+/) {
        qprint "ERROR: unable to get replica oid from '$rep_sel'.\n" .
        "   Export of replicas '@replicas' not attempted.\n";
        return 1;
    }    
    my $fn = $CFG::tmpdir . $CFG::FS . "ms_send_${dense_rep_oid}.mslock";
    my $lockstat = GetMutexLock($fn);
    if (!$lockstat) {
        print STDERR "ERROR: unable to obtain lock '$fn'.\n" .
        "   Export of replicas '@replicas' not attempted.\n";
        return 1;
    }
    # use 'chepoch -actual' to update local replica's epoch table before
    # starting the export if requested.
    if ($updateflg) {
        my $cmd = "$CFG::MultiTool chepoch -actual $rep_sel >&2";
        my $res = Do_System($cmd, __FILE__, __LINE__);
        if ($res) {
            my $warn = "WARNING: error attempting to update epoch table " .
                "from $rep_sel. Continuing.\n";
            qprint $warn;
            print STDERR $warn;
        }
    }

    my $sclass_expr = ($sclass ? "-sclass $sclass" : "");
    my $max_expr = ($maxpacket ? "-maxsize $maxpacket" : '');
    my $limit_expr = ($limit ? "-limit $limit" : "");
    my $ship_expr = ($fshipflg ? "-fship" : "-ship");
    my $compress_expr = ($do_compress ? "-compress" : "");
    

    $cmd = "$CFG::MultiTool syncreplica -export $max_expr $ship_expr" .
        " $limit_expr $compress_expr $sclass_expr $rep_sel >&2";
    
    $res = Do_System($cmd, __FILE__, __LINE__);
    if ($res) {
        print STDERR "ERROR: command '$cmd' encountered error.\n";
        qprint "ERROR: command '$cmd' encountered error.\n";
        $failcount++;
    }

    ReleaseMutex($fn);
    #
    # If the sync succeeded, update the event log.
    #
    if (!$failcount) {
	return 0;
    } else {
	#
	# Otherwise, if the sync failed:  Update the event log with
	# a pointer to the log file.
	#
	dbgprint qq/ERROR: MultiSite export error; see logfile in "$logname"\n/;
	return 1;
    }
}


sub GetReplicaEpochs {
  my ($vob, $rep) = @_;
  my %epochs = ();
  my $line;
  foreach $line
    (grep !/\.deleted/,
     split(/\n/, DoCmd("$CFG::MultiTool lsepoch replica:$rep\@$vob"))) {
      if ($line =~ m/^ oid:.*=(\d+) *\((\w+)\)/) {
	$epochs{$2} = $1;
      }
    }
  return %epochs;
}

__END__
:endofperl
ccperl -S ccperlpp.pl %0.bat %*
