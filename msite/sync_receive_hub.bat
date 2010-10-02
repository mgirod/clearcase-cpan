@rem = ' This is a perl script for NT and UNIX.
@echo off
goto endofperl
@rem ';
# -*- tab-width: 4 -*-
#---------------------------------------------------------------------------+
# Licensed Materials - Property of IBM
# Rational ClearCase
# Copyright IBM Corp. 1998, 2006. All Rights Reserved
# US Government Users Restricted Rights -
# Use, duplication or disclosure restricted by GSA ADP Schedule Contract
# with IBM Corp.
#---------------------------------------------------------------------------+

# Minor modifications for use on a hub.
# The script was renamed from the original sync_receive.bat, in order to
# avoid its being overwritten in a ClearCase update
###########################################################################
# sync_receive_hub.bat
# usage: sync_receive_hub.bat - run with "-help" for detailed usage.
#
# This script is intended to be run periodically by the ClearCase
# Job Scheduler to import packets that have been received
# from other replicas to replicas that are located on the local system.
#
# This script may also be executed interactively from the command line to
# force an import to occur immediately, either for a particular packet, for
# a particular VOB, or for a storage class.
#
# The script attempts to import to each target in each selected packet,
# securing an advisory file lock as we go, which should help serialize 
# import operations from multiple import processes and help  prevent
# possible import-collision problems. We recommend using this script instead of
# simply entering 'multitool syncreplica -import', which could potentially 
# cause collisions with other processes importing at the same time.
###########################################################################

use strict;
use Getopt::Long;
use File::Basename;
use File::Copy;

BEGIN {
    if($ =~ /^(MS(DOS|Win32)|Windows_NT)/i) {
        require Win32::TieRegistry;
    }
}
use ClearCase::CurrentVersion;

sub dbgprint;  # predeclare, routine is in MScommon.pl
sub qprint;    # predeclare

my $thisscript = $0;
if ($thisscript =~ m/(.*)(\.bat)$/) {
    $thisscript = $1;
}

my $pkg_name = 'sync_receive_hub';
$CFG::verbose = ($ENV{TRACE_SUBSYS} && ($ENV{TRACE_SUBSYS} =~ m/$pkg_name/));
# force debugging msgs by uncommenting the following and setting to 1.....
# $CFG::verbose = 1;

$CFG::quietmode = 0;

# if 'verbose' mode is on, force the debug msgs to also print to STDERR by
# uncommenting this line (forces debug msgs to appear in the log file,
# useful if you're having problems with a receipt handler).
# $CFG::stderr_msgs = 1;

use Config;
my $arch = $^O ? $^O : $main::Config{archname};
$CFG::NT = (($arch =~ /^MSWin32/) || ($arch eq 'i386-win32')) ? 1 : 0;
$CFG::UNIX = ($CFG::NT ? 0 : 1);

# CLEARCASEHOME e.v. should always be set on NT when called by ccperlpp.pl,
# so exit if it's not set. On UNIX, ClearCase::CurrentVersion::SetProductHomeEV()
# assumes /opt/rational/clearcase if user hasn't explicitly set CLEARCASEHOME 
# (or deprecated ATRIAHOME).

if ($CFG::UNIX) {
    my $producthome = &ClearCase::CurrentVersion::SetProductHomeEV;
    chdir $producthome;
    push(@INC, "$producthome/config/scheduler/tasks");
} else {
    my $producthome = $ENV{CLEARCASEHOME} or 
                      die "CLEARCASEHOME env. variable not set\n";
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
        "the MultiSite export script which includes it, 'sync_receive_hub.bat' " .
        "(version: ${CFG::MS_script_version}).\n" . 
        "Ensure that old versions of the script/require file are not in use.\n";
}

$CFG::failcount = 0;

$| = 1 if $CFG::verbose;

dbgprint "running in directory: ", DoCmd($CFG::workingdir);
dbgprint "running on architecture $arch\n";

###########################################################################
# main
#

my $usage = "$thisscript [-h] [-v vob-patt] [-w workdir] [-lo logdir]\n" .
    "    [-s sclass] [-lockwait minutes] [-t] [-o orig-host] [-q mode] \n" .
    "    [-d {file | dir}] [-a shiporder]\n" .
    "    More extensive help is available by using the '-h' command-line switch.\n";

my $helpmsg = <<ENDHELP;
=========================================================================
"sync_receive" is a general importer for incoming MultiSite packets. It may
be used interactively from the command line, run via periodic scheduler 
(ClearCase job scheduler), or it will also function as an automatic receipt 
handler triggered by the arrival of incoming packets.

A logfile is created during execution, but is normally deleted if the script 
executes successfully, and is kept if a failure is detected.

Setting the environment variable TRACE_SUBSYS to "sync_receive" will turn
on all debugging print statements (verbose mode). Using the "-trace" command
line switch will print a less-verbose set of messages. Using either tracing
method will retain the logfile even if the script is successful.

$usage
Command line options:

    -logdir directory   Specify directory to write log file. The log file will
                        be given a unique name in this directory based
                        on the process ID and timestamp. Directory must be
                        writeable by the user.
                        Defaults:
                        UNIX: /var/adm/rational/clearcase/log/sync_logs or
                              /usr/adm/rational/clearcase/log/sync_logs 
                              (platform dependent)
                        NT:   {CLEARCASEHOME}\\var\\log

    -workdir directory  Forces script to use this directory as its temporary
                        directory. Directory must exist and be writeable.
                        Default: script uses GetTmpdir() to find a temporary
                        directory to use.

    -help               Print this message and exit.

    -quiet mode         Quiet mode = 1 will suppress all non-ERROR/WARNING
                        messages normally sent to STDOUT. If set to 2,
                        all STDOUT messages are suppressed. Setting the -o
                        parameter (see below) forces the default quiet level to
                        1.
                        (Default: 0 - prints various informational messages).

    -lockwait minutes   The amount of time (in minutes) to wait on VOB 
			locks that block VOB writes. Minutes must be > 0.
			(Default: no waiting).
			NOTE: If VOB locked and lockwait NOT set, import
			will exit with error.

    -[no]trace          Display commands as they are executed, and
                        lists command-line options passed in. Forces the
                        log file to be kept.
                        (Default: "-notrace").

    -vob pattern        Specify VOB to apply imports. This parameter is a
                        pattern-match: you may either specify the whole VOB
                        name (only packets with contents for that VOB will
                        be applied); or a pattern which could match multiple 
                        VOB names. Examples:
                        -vob /vobs/dev/george1 (apply packets to 
                            /vobs/dev/george1 only)
                        -vob george (applies to any 'george' VOBs such as 
                            /vobs/dev/george1, /vobs/dev/george2,
                            /vobs/george_pvt/myvob, etc.).
                        (Default: all VOBs in any packets found).

    -sclass class       Storage class to use. (Default: all classes). This
                        parameter is also used when the script is invoked as
                        a receipt handler. Using this parameter restricts the
                        script to finding packets in the incoming bay(s) of
                        this storage class. If the storage class does not
                        have its own bay(s) set up, the default bays are used.

The following parameters are passed in automatically when the script has been
invoked as a receipt handler. The -d (datafile) parameter may be useful from
the command line if import of a particular packet or bay is desired.

When invoked as a receipt handler, note that only the '-o' parameter is
guaranteed to be passed, along with one of '-a' or '-d' (or possibly both).
The '-s' parameter is only passed if a specific storage-class has been
specified.

    -o hostname         Originating host. Only function in the script is to
                        force keeping the log file (so that all receipt
                        handler logs are kept). Specifying -o also forces the
                        script to run in "-quiet 1" mode to avoid writing 
                        excessive informational messages to the receipt
                        handler logfiles.

    -d {file | dir}     Data packet\'s full pathname. In addition to being
                        used in receipt-handler mode, this may be specified
                        interactively to import only a specific packet. This
                        may be also be used with the -vob parameter to import
                        to specific VOBs only (if the packet has multiple
                        targets).

                        If a storage-bay directory path is specified, the
                        script will attempt to import any packets found in
                        that bay.

    -s storage-class    Storage class (see -sclass above).

    -a shipping-order   Actual pathname of the new shipping order if this
                        packet is destined for another host (i.e. this
                        system is a 'hop host' for the packet). Specifying
                        a shipping order file in this parameter will cause
                        the shipping server to send this shipping order to
                        its next host destination.

Several features may be configured for the MultiSite import and export scripts by means
of a script configuration file, located at '${CFG::script_configfile}'.
To affect the import script, the config file may include the following lines:

disable_import_locking = 1

which will disable use of the import lockfile, and allow multiple imports to run
on a single replica simultaneously. Leaving this at a value of '0' (default) will
leave import locking enabled. NOTE: disabling import locking may result in import
failures due to collisions. It is recommended to leave this locking enabled unless
your configuration is experiencing high occurrence of lockfile contention.

The config file may also include the following line:

proactive_receipt_handler = 1

Normally, a receipt handler will only attempt to import the packet that it has been
invoked with. Under high-load conditions, or when packets have been split due to
maximum size restrictions, packets may arrive prematurely (before a preceeding packet
has been completely processed). Enabling 'proactive' mode causes the receipt handler to
look for any other packets that may be imported. This tends to force importing of packets
that would otherwise be 'stranded' due to premature or out-of-order delivery. The default
for this parameter is '0' (no proactive importing).
=========================================================================
ENDHELP

# get command line options figured out...
dbgprint "ARGV: " . join(' ',@ARGV) . "\n";

my $import_vob = '';
my $logdir = '';
my $help = 0;
$CFG::traceflg = 0;
my $lockwait = 0;
my $origin_host = '';
my $sclass = '';
my $actual_shiporder = '';
my $datafile = '';
my $keeplogfile = 0;
$CFG::workdir = '';
my @SAVE_ARGV = @ARGV;

my $st = GetOptions("logdir=s" => \$logdir,
                 "datafile=s" => \$datafile,
                 "origin=s"  => \$origin_host,
                 "vob=s" => \$import_vob,
                 "help!" => \$help,
                 "trace!" => \$CFG::traceflg,
                 "lockwait=i" => \$lockwait,
                 "workdir=s" => \$CFG::workdir,
                 "quiet=i" => \$CFG::quietmode,
                 "actual=s" => \$actual_shiporder,
                 "sclass=s" => \$sclass);

if ($help) {
    qprint "$helpmsg\n";
    exit 1;
}

if (!$st) {
    qprint "ERROR: error getting command line options.\nUsage: $usage\n";
    exit 1;
}

if ($lockwait < 0) {  # "lockwait" value must be > 0
    qprint "ERROR: -lockwait value must be > 0.\n";
    exit 1;
}

if ($lockwait > 0) { # "-lockwait" causes EV to be set - value must be > 0
    $ENV{"CLEARCASE_VOBLOCKWAIT"} = "$lockwait";
} else {	  # otherwise, delete EV to be sure not inheriting
    delete $ENV{"CLEARCASE_VOBLOCKWAIT"};
}

if ($origin_host && !$CFG::quietmode) { # force quiet mode on receipt handler
    $CFG::quietmode = 1;
}

dbgprint "options:\n    vob_pattern: $import_vob\n    datafile (or dir): $datafile\n" . 
    "    originating replica: $origin_host\n    workdir: $CFG::workdir\n" .
    "    logdir: $logdir\n    storage-class: $sclass\n" .
    "    lockwait: $lockwait minutes\n" .
    "    actual shipping order: $actual_shiporder\n";

if ($CFG::workdir) {
    if (! -w $CFG::workdir) {
        # the temp directory we're supposed to use is not writeable.
        print STDERR "WARNING: Working directory specified '$CFG::workdir' is not writeable; continuing...\n";
        qprint "WARNING: Working directory specified '$CFG::workdir' is not writeable; continuing...\n";
        $CFG::workdir = '';
    } else {
	# We use different tempdir EVs for Unix/Windows.
	# Override pre-existing EV only if -workdir has been specified.
	# Otherwise GetTmpdir() will make a guess at an appropriate value.
        if ($CFG::UNIX) {
            $ENV{TMPDIR} = $CFG::workdir if $CFG::workdir;
        } else {
            $ENV{TMP} = $CFG::workdir if $CFG::workdir;
        }
    }
}    

#
# Validate that the log directory actually exists...
#
$logdir = ValidateLogDir($logdir);

#
# Redirect STDERR to a uniquely named log file
#
my $logname = RedirectSTDERRtoLog ($logdir, 'recv');

print STDERR "=========================================================\n" .
    "Starting MultiSite import from script '$thisscript' at " .
    MakeTimestamp() . "\n";

print STDERR "Actual arguments: " . join(' ',@SAVE_ARGV) . "\n";

print STDERR "options:\n    vob_pattern: '$import_vob'\n    datafile (or dir): '$datafile'\n" . 
    "    originating host: '$origin_host'\n    workdir:    '$CFG::workdir'\n" .
    "    logdir: '$logdir'\n    storage-class: '$sclass'\n" .
    "    lockwait: '$lockwait minutes'\n" .
    "    actual shipping order: '$actual_shiporder'\n";

undef %CFG::pktlist;
my ($cmd, $res);

my $done = 0;   # 'import done' flag

if ($origin_host && -f $datafile) {    # indicates we are a receipt handler, set 'rec handler' flag
    $CFG::receipt_handler = 1;
} else {
    $CFG::receipt_handler = 0;
}

if ($datafile) {
    # if set, we are running in 'receipt-handler' (single datafile) mode.
    # Or, someone could have passed in a storage-bay directory!
    if (-d $datafile) { # it's really a storage bay! compress the whole thing.
        UnCompressDir($datafile);
    } elsif ($datafile =~ m/(.*)\.(gz|gz(\d*)(_{0,1})(\d*))$/i) { 
	# compress'd file; uncompress it
        my $name = $1;
        my $ext = $2;
	my $success;
	my $name_gz = $datafile;

	($name_gz, $success) = RenameGZFileName($name, $ext);
	if ($success == 0) {
	    # We can't trust the new filename
	    die "Unable to rename packet $datafile to $name_gz: $!\n";
        }
        # uncompress the file.
        $cmd = qq/$CFG::compress_pgm -f -d "$name_gz" < $CFG::nulldevice/;

        my $res = Do_System($cmd, __FILE__, __LINE__);
        if ($res != 0) {
            qprint "WARNING: uncompress failed for file $datafile.\n";
        }
	# Strip the ".gz" from $name_gz so that the lspacket works
	if ($name_gz =~ m/(.*)\.gz/) {
            $datafile = $1;	
        } else {
	    $datafile = $name;
        }
    }    
    GetLspacketInfo($datafile, "incoming");
} elsif (!$origin_host) {
    # we are NOT a receipt handler if $origin_host is NULL.
    # use cmd line params to figure list of replicas/packets to import

    # locate shipping bays for the storage class (all bays if $sclass is null)
    # and decompress any *.gz files. Pick up any packets that we need
    # and store their names in %CFG::pkt_list.

    my (@bays) = GetAllBays($sclass);
    my $dir;
    # scan thru the shipping bays. Need to add the "incoming" subdirectory
    foreach $dir (@bays) {
        dbgprint "Shipping bay returned by GetAllBays(): $dir\n";
        my $inbay = $dir . $CFG::FS . "incoming";
        UnCompressDir($inbay);
        GetLspacketInfo($inbay, "incoming");
    }
} else {
    # if we're here, we ARE a receipt handler and NO $datafile is specified.
    # Should indicate that only a shipping order (-a parameter) was passed, so
    # we don't need to look for packets to import. Set the 'import done' flag.
    $done = 1;
}

##############
# begin import
##############

if (!scalar (keys %CFG::pktlist) && !$done) {
    $done = 1;
    print STDERR "INFO: no valid packets were found to import.\n";
    qprint "INFO: no valid packets were found to import.\n";
}

# make a list of packets that contain stuff for targets we DON'T want to update
# (i.e. if we specify '-vob xxx' on the cmd line, but our packet has contents
# for vobs xxx, yyy, and zzz we only import to xxx, and won't delete the
# packet).
my %partial_pktflag;

my $tmp_failcount;

while (!$done) {
    my $pkt;
    my $success_count = 0;
    $tmp_failcount = 0;   # keep track of number of failures on each pass.

    foreach $pkt (sort keys %CFG::pktlist) {
        # Attempt to import each packet one VOB at a time, securing an advisory
        # mutex lock on the replica/tag before attempting.
        # The packets we found may not be applicable in the order we found
        # them, so we'll keep going thru the list of packets until they stop
        # importing successfully, or we run out of packets to try.
        #
        # If each target in a packet appears to have imported correctly, we'll
        # issue a 'syncreplica import $pkt_name', which should find that all
        # of the packet has already been applied, and delete the packet. This
        # should cause no changes to the target VOB(s), so we won't get a lock
        # beforehand. This method should be safer than just deleting the file, 
        # just in case we missed an error along the way or the packet was not
        # completely applied for some reason.
        dbgprint "pkt found: $pkt\n";

        my @fraglist = ();
        my $importlist;
        my @fragdirs = ();
        my $frag;
        my $ref_list;
        my $all_arrived = 0;
        my $dup_frag = 0;

        # get pkt fragment info
        if ($CFG::pktlist{$pkt}[2] > 1) {
            #dbgprint "\n\npkt is a fragment: $pkt\n";
            
            # if the pkt_frags sub dir does not exist, create it
            if (!(-d $CFG::frag_dir)) { 
                # create frag_dir
                my $res = mkdir $CFG::frag_dir;
                if (!$res) {
                    qprint "ERROR: unable to create packet fragment directory $CFG::frag_dir: $!\n";
                    die "ERROR: can't create packet fragment directory: $CFG::frag_dir: $!\n";
                }
            }
            
            if (!(-w $CFG::frag_dir)) {
                qprint STDERR "ERROR: packet fragment directory not writeable: $CFG::frag_dir\n";
                die "ERROR: packet fragment directory not writeable: $CFG::frag_dir\n";
            }

            
            $ref_list = "$CFG::frag_dir" . $CFG::FS . "$CFG::pktlist{$pkt}[3]";
            #dbgprint "INFO: ref_list is $ref_list\n";

            # check if this is the last fragment we were waiting on to import
            # the logical packet. (also check if this fragment is already in
            # the reference list)
            if (-e $ref_list) {
                my $idx;
                push (@fraglist, "$pkt");
                
                open FH, "< $ref_list";
                while (<FH>) {
                    chomp;
                    ($idx, $frag) = split /\s+/, $_, 2;
                    #dbgprint "INFO: read from ref_list: $frag\n";
                    if ($idx == $CFG::pktlist{$pkt}[1]) {
                        # the fragment is already in the ref_list
                        $dup_frag = 1;
                        next;
                    }
                    push (@fraglist, "$frag");
                    if (($#fraglist + 1) >= $CFG::pktlist{$pkt}[2]) {
                        $all_arrived = 1;
                        next;
                    }
                }
                close FH;
            }

            # if its not a duplicate fragment, we need to add it to the
            # reference list
            if (!$dup_frag) {
                # add fargment to ref_list
                if ($] > 5.001) {
                    # attempt to open/create ref_list
                    $res = sysopen(FH, $ref_list, Fcntl::O_WRONLY() | 
                                                  Fcntl::O_APPEND() | 
                                                  Fcntl::O_CREAT(), 0666);
                } else {
                    $res = open FH, ">> $ref_list";
                }
                if (!$res) {
                    dbgprint "Error in fragment reference file open call; $ref_list NOT created!\n";
                    die "ERROR: unable to open/create fragment reference file '$ref_list'\n";
                }
                #for (my $try = 0; $try < 3; $try++) {
                #    dbgprint "Trying to lock fragment reference file: $ref_list\n";
                #    if (flock(FH, LOCK_EX)) {
                #        next;
                #    } 
                #    # wait for 5 sec and try again
                #    sleep(5);
                #}

                # add fragment to ref_list
                #dbgprint "INFO: adding $pkt to ref list\n";
                print FH "$CFG::pktlist{$pkt}[1] $pkt\n";

                #flock(FH, LOCK_UN) 
                #    or dbgprint "Warning: Could not unlock fragment" .
                #    " reference file: $ref_list\n";
                close FH;
            }

            # if logical pkt is now complete, import it. 
            if ($all_arrived) {
                #dbgprint "INFO: All fragments arrived.. import $#fraglist +1 fragments\n";
                foreach $frag (@fraglist) {
                    my $parentdir = dirname($frag);
                    my $subdir = "$parentdir" . $CFG::FS . "$CFG::pktlist{$pkt}[3]";
                    if (!(-d $subdir)) { # if a subdir does not exist 
                        # create subdir
                        dbgprint "INFO: Making sub-dir to import: $subdir\n"; 
                        my $res = mkdir $subdir;
                        if (!$res) {
                            dbgprint "ERROR: unable to create packet fragment directory $subdir: $!\n";
                            qprint "ERROR: unable to create packet fragment directory $subdir: $!\n";
                            die "ERROR: can't create packet fragment directory: $subdir: $!\n";
                        }
                        push(@fragdirs, "$subdir");
                        $importlist .= "$subdir ";
                        #dbgprint "INFO: Adding $subdir to import-list\n";
                    }
                    
                    if (-w $subdir) {
                        #dbgprint "INFO: adding $frag to $subdir\n";
                        move($frag, $subdir . $CFG::FS . basename($frag));
                    } else {
                        dbgprint "ERROR: packet fragment directory not writeable: $subdir\n";
                        print STDERR "ERROR: packet fragment directory not writeable: $subdir\n";
                        die "ERROR: packet fragment directory not writeable: $subdir\n";
                    }
                    
                }
                
                # remove the trailing space in $importlist
                $importlist =~ s/\s+$//;
                dbgprint "INFO: *** all fragments found.. importing $importlist\n";
                # next;
            } else {
                # is sll fragments have not arrived, dont bother even
                # attempting the import so just go to the next pkt 
                next;
            }
        } # end of fragment handling
        else { 
            push (@fraglist, $pkt);
            $importlist = $pkt;
        }
        
        my @replist = split('\|', $CFG::pktlist{$pkt}[0]);
        foreach $frag (@fraglist) {
            $CFG::pktlist{$frag}[0] = '';
        }
        my $rep_tag;
        foreach $rep_tag (@replist) {
            # get replica/tag pair off the list
            my ($rep,$tag) = split(";", $rep_tag);
            dbgprint "found rep/tag: $rep_tag $rep $tag\n";

            # if "-vob" specified on cmd line, see if we want to import
            # to this target.
            my $vob_qm = quotemeta $import_vob;
            if ($import_vob && !($tag =~ m/$vob_qm/)) {
                $partial_pktflag{$pkt} = 1;
                next;
            }

            # use MS script config file setting to turn off
            # import locking if requested. 
            $CFG::disable_locking = $CFG::MScfg_disable_import_locking;

            my $dense_rep_oid = GetDenseOid ("replica:$rep\@$tag");
            if ($dense_rep_oid !~ m/([0-9][a-f])+/) {
               qprint "ERROR: unable to get replica oid from '$rep\@$tag'.\n" .
                      "   Export of replica '$rep' not attempted.\n";
               return 1;
            }

            my $fn = GetTmpdir() . $CFG::FS . "ms_recv_${dense_rep_oid}.mslock";
            my $lockstat = GetMutexLock($fn);
            if (!$lockstat) {
                print STDERR "Unable to get lock for replica: $rep VOB: $tag.\n" .
                    "    Import will not be attempted for this replica. Continuing...\n";
                $tmp_failcount++;
                # record the packet file in the list to avoid import racing
                foreach $frag (@fraglist) {
                    if (-e $frag) {
                        $CFG::pktlist{$frag}[0] .= "$rep;$tag\|";
                    }
                }
                next;
            }
            # do the import for one target VOB if we got the lock, or all we can if in 'proactive' mode.
            # proactive mode is ONLY used when we believe that we've been invoked as a receipt handler.
            #
	    my $skip_ship = 0;
            if ($CFG::MScfg_proactive_receipt_handler && $CFG::receipt_handler) {
                $cmd = qq/$CFG::MultiTool syncreplica -import -receive -invob $tag/;
		if ($actual_shiporder and ($sclass ne 'express')) {
		  dbgprint "Do not ship this: $actual_shiporder,\nif the import is successful. ".
		    "It would be redundant with the sync packet produced later by the hub\n";
		  $skip_ship = 1;
		}
            } else {
                $cmd = qq/$CFG::MultiTool syncreplica -import -invob $tag "$importlist"/;
            }
            $res = Do_System($cmd, __FILE__, __LINE__);
            if ($res == 0) {
                if ($CFG::NT) {
                    $cmd = "$CFG::imsglog sync_import evtlog";
                    $res = Do_System($cmd, __FILE__, __LINE__);
                }
                $success_count++;
                print STDERR "SUCCESSFUL MultiSite import to $rep $tag.\n";
		if ($skip_ship) {
		  dbgprint "About to delete $pkt and $actual_shiporder\n";
		  if (open SHORDER, "<$actual_shiporder") {
		    my ($line, $pkt) = ();
		    foreach $line (<SHORDER>) {
		      next if $line =~ /\s*\#.*/;    # skip comment lines with #
		      if ($line =~ /^\%LOCAL-DATA-PATH \"(.*)\"/) {
			$pkt = $1;
			last;
		      }
		    }
		    close SHORDER;
		    if ($pkt) {
		      unlink $pkt, $actual_shiporder;
		      dbgprint "Deleted $pkt and $actual_shiporder\n";
		      $actual_shiporder = 0;
		    } else {
		      print STDERR "Failed to match the local data path in the shipping order:" .
			" $actual_shiporder.\n";
		    }
		  } else {
		    print STDERR "Failed to open $actual_shiporder, for skipping its shipping\n";
		  }
		}
            } else {
                #
                # The sync failed.  Update the event log with a pointer to the log file
                #
                if ($CFG::NT) {
                    $cmd = "$CFG::imsglog sync_import evtlog \"$logname\"";
                    $res = Do_System($cmd, __FILE__, __LINE__);
                }
                foreach $frag (@fraglist) {
                    if (-e $frag) {
                        $CFG::pktlist{$frag}[0] .= "$rep;$tag\|";
                    }
                }
                print STDERR "INFO: attempted MultiSite import to $rep $tag failed.\n";
                dbgprint "INFO: attempted import to $rep $tag failed." .
                    " See logfile in \"$logname\"\n";
                $tmp_failcount++;
            }
            ReleaseMutex($fn) if $lockstat;
        }

        # See if the packet may be removed - if all targets in the 
        # packet applied correctly, the hash pktlist{$pkt}[0] will be NULL.
        # If the partial_pktflag{} hash is set, it means that all targets in
        # the packet were not eligible for import, so we'll keep the packet.
        # But if there was only a single target in the packet *and* it was
        # imported successfully, the packet will be gone already, so we
        # don't need to do any more.
        if (!$CFG::pktlist{$pkt}[0]) {  # no targets to retry?
            foreach $frag (@fraglist) {
                undef $CFG::pktlist{$frag}[0];
            }
            if (-e $pkt && !$partial_pktflag{$pkt}) {
                dbgprint "attempt to remove the packet $pkt....\n";
                # all targets in the packet should have been applied, so
                # this should be a no-op except to delete the packet.
                $cmd = qq/$CFG::MultiTool syncreplica -import "$importlist"/;
                $res = Do_System($cmd, __FILE__, __LINE__);
                
                # remove the ref_list if this was a fragmented import
                if ($CFG::pktlist{$pkt}[2] > 1) { # if these were fragments
                    dbgprint "INFO: removing $ref_list\n";
                    unlink $ref_list;
                }
            } else {
                if ($CFG::pktlist{$pkt}[2] > 1) { # if these were fragments
                    # move all fragments back to the parent dirs
                    my $subdir;
                    foreach $frag (@fraglist) {
                        #dbgprint "INFO: Cleanup: moving file $frag\n";
                        my $parentdir = dirname($frag);
                        $subdir = "$parentdir" . $CFG::FS . "$CFG::pktlist{$pkt}[3]";
                        move($subdir . $CFG::FS . basename($frag), $frag);
                    }
                    #dbgprint "INFO: Cleanup: removing dir $subdir\n";
                    rmdir $subdir;
                }
            }
        }
    }

    # if any operation succeeded on this pass, it may have been the import that
    # was blocking another from succeeding, so we'll try going thru the list
    # again. If nothing succeeded this pass, we're not likely to succeed by
    # trying again, so we're quitting.
    if (!$success_count) {
        $done = 1;
    }
    # only save the # of failures that occurred on the FINAL pass, since previously
    # detected errors may have been successful on re-tries.
    $CFG::failcount = $tmp_failcount;
}

# was a shipping order specified on the cmd line? If so, try to launch it 
if ($actual_shiporder) {
    if (-e $actual_shiporder) {
	my $cmd = qq/$CFG::shipping_server "$actual_shiporder"/;
	$res = Do_System($cmd, __FILE__, __LINE__);
	if ($res == 0) {
	    if ($CFG::NT) {
		$cmd = "$CFG::imsglog sync_import evtlog";
		$res = Do_System($cmd, __FILE__, __LINE__);
	    }
	} else {
	    #
	    # The shipping server failed.  Update the event log with a pointer to the log file
	    #
            $CFG::failcount++;
	    if ($CFG::NT) {
		$cmd = "$CFG::imsglog sync_import evtlog \"$logname\"";
		$res = Do_System($cmd, __FILE__, __LINE__);
	    }
	    print STDERR "ERROR: attempt to forward shipping order $actual_shiporder failed.\n";
            qprint "ERROR: attempt to forward shipping order $actual_shiporder failed.\n";
	    dbgprint "ERROR: attempt to forward shipping order $actual_shiporder failed." .
		" See logfile in \"$logname\"\n";
        }
    } else {
        # can't find the shipping order specified. It may have already been sent if
        # the scheduler or user fired off a 'shipping_order -poll'. Print a warning,
        # but don't flag as an error.
        dbgprint "WARNING: shipping order $actual_shiporder not found\n";
        print STDERR "WARNING: shipping order $actual_shiporder not found (perhaps previously sent?).\n";
        qprint "WARNING: shipping order $actual_shiporder not found (perhaps previously sent?).\n";
    }
}

if ($CFG::traceflg || $CFG::verbose || $CFG::failcount) {
    $keeplogfile = 1;
}

if ($CFG::failcount) {
    print STDERR qq/\nERROR: $CFG::failcount import error(s) detected!\nLog is "$logname"\n/;
    qprint qq/\nERROR: $CFG::failcount import error(s). See logfile at: "$logname"\n/;
} else {
    qprint "\nSUCCESSFUL COMPLETION of MultiSite import.\n";
    print STDERR "\nSUCCESSFUL COMPLETION of MultiSite import.\n";
    if ($keeplogfile) {
        qprint "See logfile at: $logname.\n";
    }
}
close(STDOUT);
close(STDERR);
if (!$keeplogfile) {
    unlink $logname;
}
exit $CFG::failcount;

sub GetLspacketReps {
    # Parse the output from 'lspacket -long' to generate a list of replicas
    # that this packet may be applied to.
    # NOTE: lspacket -long should print a "*" on 'importable' packets, but this
    # is currently broken. We'll try to import ALL packets until this is fixed.
    # Two (optional) inputs:
    #   packet or shipping bay path
    #   "incoming" or "outgoing" to restrict what packets are parsed. if null,
    #       do all packets we find.
    # Returns: a hash with the packet's path as the key; the value is a concat
    # string containing replica/tag pairs that this packet applies to. e.g.:
    #    $hash{"/tmp/incoming/packet_1"} = "rep1;tag1|rep2;tag2|rep3;tag3....";
    # Also updates a global hash %CFG::pktlist{} so we can accumulate replicas
    # from multiple packets.

    my %pktlist;
    my $do_inbays = 1;   # by default look at both incoming and outgoing bays
    my $do_outbays = 1;
    my $pkt_name = shift;  # if null, get all packets
    my $pkt_type = shift;
    if (!$pkt_type) {
        # default if not specified
    } elsif ($pkt_type eq 'outgoing') {
        $do_inbays = 0;
    } else { # anything else? we'll look at the incoming bays.
        $do_outbays = 0;
    }

    my $pkt_spec = ($pkt_name ? qq/"$pkt_name"/ : '');
    my $lspacket = DoCmdNoTranslate("$CFG::MultiTool lspacket -long $pkt_spec");
    my @lines = split("\n", $lspacket);
    my $line;
    my @replica_list = ();
    my $checkline = 0;
    my $p_type = '';
    my $pkt_path = '';
    foreach $line (@lines) {
        # collect replica lines after 'packet intended' message; turn off
        # collection after hitting 'originating replica...'
        #
        # parse the packet path name for /incoming/ or /outgoing/ to get type.
        #dbgprint "line:$line\n";
        if ($line =~ /^Packet is:\s+(.*)$/) {

            #$p_type = 'incoming';
            $pkt_path = $1;
            if ($line =~ m#outgoing#) {
                $p_type = 'outgoing';
            } elsif ($line =~ m#incoming#) {
                $p_type = 'incoming';
            } else {
                # might get here if importing a packet not in a std. bay.
                # Assume it's incoming.
            }
            dbgprint "$p_type Packet name found: $pkt_path\n";
        }

        if ($line =~ /Packet type: Replica Creation/) {
            # don't try simple import on a replica creation packet!
            dbgprint "packet is a replica creation packet; ignoring it.\n";
            $pkt_path = '';
        }

        next if (!$pkt_path);
        next if ($p_type eq 'outgoing' && !$do_outbays);
        next if ($p_type eq 'incoming' && !$do_inbays);

        if ($line =~ /^Packet intended for the following targets/) {
            $checkline = 1;
            next;
        } elsif ($line =~ /^Originating replica is/) {
            $checkline = 0;
            next;
        } elsif ($checkline) {
            # found a replica line, parse out repl. name and tag values, and
            # add to the list. Don't match lines unless have "[ local to..."
            dbgprint "replica line from lspacket: $line\n";
            if ($line =~ m/\s+([\S]+)\s+\[.*\]\s+[\*\s]*tag:\s+(.+)$/) {
                dbgprint "local replica found: $1 tag: $2\n";
                if ($1 && $2) {
                    $pktlist{$pkt_path}[0] .= "$1;$2\|";
                    $CFG::pktlist{$pkt_path}[0] .= "$1;$2\|";
                 }
            } else {
                # replica found, but not on this host (no "[ local to... ]")
                # so ignore it.
            }
        }
    }
    return %pktlist;
}

sub GetAllBays {
    my @bays;
    my $sclass = shift;
    if ($CFG::NT) {
        my $hkey = Win32::TieRegistry->new("LMachine\\$RGY::ms_st_regy\\",{Access=>'KEY_READ'});
        dbgprint "hkey: $hkey\n";
        if($hkey) {
            my @sclass = ($sclass ne '') ? ($sclass) : $hkey->SubKeyNames();
            foreach $sclass (@sclass) {
                dbgprint "foreach storage class $sclass\n";
                my $bay = GetBaysFromSClass($sclass);
                push(@bays, $bay) if($bay ne '');
                undef $hkey;
            }
        }
    } else {
        my $cmd = qq@grep STORAGE-BAY $ENV{CLEARCASEHOME}/config/services/shipping.conf | grep -v "#" @;
        my $baylist = DoCmd($cmd);
        my @bay_lines = split("\n", $baylist);
        my $bay_info;
        foreach $bay_info (@bay_lines) {
            my ($sb, $class, $bay_path) = split(/\s+/, $bay_info, 3);
            dbgprint "$class path: $bay_path\n";
            if (!$sclass || $sclass eq $class) {
                push(@bays, $bay_path);
            }
        }
    }
    dbgprint "for storage class '" . ($sclass ? $sclass : "(all classes)") . "' found bays: " .
        join (" ", @bays) . "\n";
    return @bays;
}

sub GetBaysFromSClass {
    # return bays for a given shipping class (NT only!)
    my $sclass = shift;

    dbgprint "GetBaysFromSClass ($sclass)\n";
    my $result;
    my $keyname = "LMachine\\$RGY::ms_st_regy\\$sclass";
    dbgprint "looking up registry key: $keyname\n";
    my $key = Win32::TieRegistry->new($keyname, {Access=>'KEY_READ'});
    dbgprint "key found is: $key\n";
    if (!defined $key) {
        qprint "ERROR: could not get registry key $keyname!\n";
        undef $result;
    } else {
        my $value = $key->GetValue("StorageBay");
        dbgprint "StorageBay value is: $value\n";
        if($value eq '') {
            qprint "WARNING: Unable to get storage bay for class $sclass\n";
        } else {
            $result = $value;
        }
        undef $key;
    }

    return $result;
}

sub GetLspacketInfo {
    # Parse the output from 'lspacket -long' to get the following information:
    # - a list of replicas that this packet may be applied to.
    # - Fragment sequence number
    # - Total number of fragments in the logical packet
    # - Logical packet UUID
    # NOTE: This routine can be updated in the future to get other fields
    #
    # NOTE: lspacket -long should print a "*" on 'importable' packets, but this
    # is currently broken. We'll try to import ALL packets until this is fixed.
    # Two (optional) inputs:
    #  - packet or shipping bay path
    #  - "incoming" or "outgoing" to restrict what packets are parsed. if null,
    #    do all packets we find.
    # Returns: a hash with the packet's path as the key; the data is a list
    # with its members being (in that order)
    # [0]- a concat string containing replica/tag pairs that this packet applies to. 
    # [1]- Fragment sequence number
    # [2]- Total number of fragments in the logical packet 
    # [3]- Logical packet UUID 
    # e.g.: $hash{"/tmp/incoming/packet_1"} =
    # "rep1;tag1|rep2;tag2|rep3;tag3...", 3, 5, fd6824ec.c22111d9.8337.00:01:80:fd:d8:5d";
    # Also updates a global hash %CFG::pktlist{} so we can accumulate information
    # from multiple packets.

    my %pktlist;
    my $do_inbays = 1;   # by default look at both incoming and outgoing bays 
    my $do_outbays = 1;
    my $pkt_name = shift;  # if null, get all packets
    my $pkt_type = shift;
    if (!$pkt_type) {
        # default if not specified
    } elsif ($pkt_type eq 'outgoing') {
        $do_inbays = 0;
    } else { # anything else? we'll look at the incoming bays.
        $do_outbays = 0;
    }

    my $pkt_spec = ($pkt_name ? qq/"$pkt_name"/ : '');

    my $lspacket = DoCmdNoTranslate("$CFG::MultiTool lspacket -long $pkt_spec");

    my @lines = split("\n", $lspacket);
    my $line;
    my @replica_list = ();
    my $checkline = 0;
    my $p_type = '';
    my $pkt_path = '';
    my $total_frags = 0;
    my $frag_seq = 0;
    my $log_pkt_uuid;

    foreach $line (@lines) {
        # collect replica lines after 'packet intended' message; turn off
        # collection after hitting 'originating replica...'
        #
        # parse the packet path name for /incoming/ or /outgoing/ to get type.
        #dbgprint "line:$line\n";
        if ($line =~ /^Packet is:\s+(.*)$/) {

            #$p_type = 'incoming';
            $pkt_path = $1;
            if ($line =~ m#outgoing#) {
                $p_type = 'outgoing';
            } elsif ($line =~ m#incoming#) {
                $p_type = 'incoming';
            } else {
                # might get here if importing a packet not in a std. bay.
                # Assume it's incoming.
            }
            dbgprint "$p_type Packet name found: $pkt_path\n";
        }

        if ($line =~ /Packet type: Replica Creation/) {
            # don't try simple import on a replica creation packet!
            dbgprint "packet is a replica creation packet; ignoring it.\n";
            $pkt_path = '';
        }

        next if (!$pkt_path);
        next if ($p_type eq 'outgoing' && !$do_outbays);
        next if ($p_type eq 'incoming' && !$do_inbays);

        if ($line =~ /^Packet intended for the following targets/) {
            $checkline = 1;
            next;
        } elsif ($line =~ /^Originating replica is/) {
            $checkline = 0;
            next;
        } elsif ($checkline) {
            # found a replica line, parse out repl. name and tag values, and
            # add to the list. Don't match lines unless have "[ local to..."
            dbgprint "replica line from lspacket: $line\n";
            if ($line =~ m/\s+([\S]+)\s+\[.*\]\s+[\*\s]*tag:\s+(.+)$/) {
                dbgprint "local replica found: $1 tag: $2\n";
                if ($1 && $2) {
                    $pktlist{$pkt_path}[0] .= "$1;$2\|";
                    $CFG::pktlist{$pkt_path}[0] .= "$1;$2\|";
                 }
            } else {
                # replica found, but not on this host (no "[ local to... ]")
                # so ignore it.
            }
        } elsif ($line =~ /^Packet fragment:\s+(\d+) of (\d+)$/) {
            dbgprint "Packet fragment: $1 of $2\n";
            if ($1 && $2) {
                $pktlist{$pkt_path}[1] = $1;
                $CFG::pktlist{$pkt_path}[1] = $1;
                $pktlist{$pkt_path}[2] = $2;
                $CFG::pktlist{$pkt_path}[2] = $2;
            }
        } elsif ($line =~ /^Packet identifier is:\s+(([0-9a-zA-Z\:\.])+)$/) {
            my $d_oid = OidToDenseOid($1);
            dbgprint "Logical Packet UUID: $d_oid\n";
            if ($d_oid) {
                $pktlist{$pkt_path}[3] = $d_oid;
                $CFG::pktlist{$pkt_path}[3] = $d_oid;
            }            
        }
    }
    return %pktlist;
}


__END__
:endofperl
ccperl -S ccperlpp.pl %0.bat %*
