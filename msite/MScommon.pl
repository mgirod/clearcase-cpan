# -*- tab-width: 4 -*-
###########################################################################
#---------------------------------------------------------------------------+
# Licensed Materials - Property of IBM
# Rational ClearCase
# Copyright IBM Corp. 1998, 2006. All Rights Reserved
# US Government Users Restricted Rights -
# Use, duplication or disclosure restricted by GSA ADP Schedule Contract
# with IBM Corp.
#---------------------------------------------------------------------------+

use strict;
use Config;
use Fcntl;

BEGIN { 
	# configuration items - OS dependent
	if (!($CFG::UNIX || $CFG::NT)) {
		my $arch = $^O ? $^O : $main::Config{archname};
		$CFG::NT = (($arch =~ /^MSWin32/) || ($arch eq 'i386-win32')) ? 1 : 0;
	}
}

if ($CFG::NT) {
    $CFG::FS = qq/\\/;
    $CFG::UNIX = 0;
} else {
	if (!exists $ENV{CLEARCASEHOME}) {
        # check for (now deprecated) ATRIAHOME e.v.; use if set.
        if (exists $ENV{ATRIAHOME} ) {
			$ENV{CLEARCASEHOME} = $ENV{ATRIAHOME};
		} else {
            $ENV{CLEARCASEHOME} = "/opt/rational/clearcase";
        }
    }
    $CFG::FS = qq@\/@;
    $CFG::UNIX = 1;
}

# common tool definitions
# add "./" or ".\" to executable paths in case the user does not have "."
# on their PATH
my $dotslash = ".${CFG::FS}";
my $bindir = "${dotslash}bin" . $CFG::FS;
my $etcdir = "${dotslash}etc" . $CFG::FS;

$CFG::compress_pgm = "${etcdir}ccgzip";
$CFG::MultiTool = "${bindir}multitool";
$CFG::ClearTool = "${bindir}cleartool";

if ($CFG::NT) {
	$CFG::logdir = "$ENV{CLEARCASEHOME}${CFG::FS}var${CFG::FS}log";
    $CFG::MkOrder = "${bindir}mkorder";
    $CFG::imsglog = "${bindir}imsglog";
    $CFG::shipping_server = "${bindir}shipping_server";
    $CFG::pid_exists = "${etcdir}${CFG::FS}utils${CFG::FS}pid_exists";
    $CFG::workingdir = "cd";
    $CFG::nulldevice = "nul:";
    $CFG::script_configfile = "$ENV{CLEARCASEHOME}${CFG::FS}var${CFG::FS}config${CFG::FS}MSimport_export.conf";
    $CFG::alternate_hostsfile = "";
    $CFG::frag_dir = "$ENV{CLEARCASEHOME}${CFG::FS}var${CFG::FS}pkt_frags";
} else {
    my $var_prod_dir = "/var/adm/rational/clearcase";
    $CFG::logdir = "${var_prod_dir}/log/sync_logs";
    $CFG::MkOrder = "${etcdir}mkorder";
    $CFG::imsglog = "${etcdir}imsglog";
    $CFG::shipping_server = "${etcdir}shipping_server";
    $CFG::ps = "/bin/ps";
    $CFG::workingdir = "/bin/pwd";
    $CFG::nulldevice = "/dev/null";
    $CFG::script_configfile = "${var_prod_dir}/config/MSimport_export.conf";
    $CFG::alternate_hostsfile = "${var_prod_dir}/config/alternate_hostnames";
    $CFG::frag_dir = "${var_prod_dir}/pkt_frags";
}

sub qprint;
sub dbgprint;
sub print2;

require 'ms_utils.pl';

# define this version - used by including scripts to check against mismatch
$CFG::MScommon_version = "7.0.0.0";

if (!$CFG::MS_script_version) {
    $CFG::MS_script_version = "(unknown older version)";
}

if ($CFG::MScommon_version ne $CFG::MS_script_version) {
    die "Version mismatch between require file 'MScommon.pl' (version: ${CFG::MScommon_version}) and \n" .
        "the MultiSite import or export script which includes it, 'sync_receive.bat' or 'sync_export_list.bat'" .
        " (script version: ${CFG::MS_script_version}).\n" . 
		"Ensure that old versions of the script/require file are not in use.\n";
}

$CFG::MScfg_proactive_receipt_handler = 0;
$CFG::MScfg_disable_export_locking = 0;
$CFG::MScfg_disable_import_locking = 0;

if (-e $CFG::script_configfile) {
    open CFGFILE, "< ${CFG::script_configfile}" 
        or die "Configfile ${CFG::script_configfile} exists but could not be opened.\n";
    my $line;
    foreach $line (<CFGFILE>) {
        next if $line =~ /\s*\#.*/;    # skip comment lines with #
        if ($line =~ /\s*proactive_receipt_handler\s*\=\s*(\d*)/i) {
            $CFG::MScfg_proactive_receipt_handler = $1;
        }
        if ($line =~ /\s*disable_export_locking\s*\=\s*(\d*)/i) {
            $CFG::MScfg_disable_export_locking = $1;
        }
        if ($line =~ /\s*disable_import_locking\s*\=\s*(\d*)/i) {
            $CFG::MScfg_disable_import_locking = $1;
        }
    }
}

sub ConsPktFileName {
    # Construct a packet name....
    #
    my $repname = shift;
    my $dest_replica = shift;
    if (!$repname) {
        $repname = 'zz';
    }
    $CFG::packetsequence++;
    return "sync-" . MakeTimestamp() . "-$$-" . $CFG::packetsequence . "-" .
        substr($repname, 0, 40) . "-" . substr($dest_replica, 0, 40);
}

sub UnCompressDir {
    my $dir = shift;
    my (@files, $file, $filename, $res, $cmd);

    dbgprint "Compress ($dir)\n";

    opendir(DIR, $dir);
    @files = grep(/\.gz/, readdir(DIR));
    foreach $file (@files) {   
		my $success;
		my $name;
		my $ext;

		$filename = $dir . $CFG::FS . $file;
		
		if ($filename =~ m/(.*)\.(gz|gz(\d*)(_{0,1})(\d*))$/) {
			$name = $1;
			$ext = $2;

			($filename, $success) = RenameGZFileName($name, $ext);
			if ($success == 0) {
				# We can't trust the new filename
				qprint "WARNING: rename failed for $filename: $!\nContinuing.\n";
			} else {
				$filename = qq/"$filename"/;
				$cmd = "$CFG::compress_pgm -d $filename < $CFG::nulldevice";
				$res = Do_System($cmd, __FILE__, __LINE__);
				if ($res != 0) {
					qprint "WARNING: uncompress failed for file $filename. Continuing.\n";
				}
			} # end if success
		}
		closedir(DIR);
	} # end for
}

sub GetMutexLock {
    # acquire an advisory lock by attempting to create file; if other process
    # has created the file already, attempt to read file to get the PID of the 
    # process that created the file and see if it's still around. Delete the
    # lock if it's stale, and try to acquire it again. If the other process 
    # seems to be still active, return a failure. This could fail improperly
    # if a PID is reused after a lock file is abandoned, but this should
    # be rare. If this is a problem, consider checking the timestamp in the lock 
    # and declaring the lock stale after XXX amount of time.

    if ($CFG::disable_locking) { # recommend this for EXPORTS only....
        return 1;
    }

    my $exists = 0;
    my $lockfile = shift;
    dbgprint "try to create mutex lock: $lockfile... ";
    if ($] > 5.001) {
        if (!(sysopen FLAG, $lockfile, Fcntl::O_RDWR() | Fcntl::O_CREAT() | 
                                       Fcntl::O_EXCL(), 0666)) {
            $exists = 1;
        }
        if ($exists && !(-e $lockfile)) {
            # normally, failure indicates that lockfile already exists; if it doesn't,
            # something else unusual is happening.
            dbgprint "Error in lockfile open call; $lockfile NOT created!\n";
            print STDERR "ERROR: unable to open/create lockfile '$lockfile'; lock not granted.\n";
            return 0;
        }
    } else {
        if (-e $lockfile) {
            $exists = 1;
        } else {
            open FLAG, ">$lockfile";
        }
    }
    if ($exists) {
        dbgprint "lock exists; see if locking PID still active...\n";
        my $fail = 0;
        my $buff;
		if ($] > 5.001) {
		    (sysopen FLAG, $lockfile, Fcntl::O_RDONLY(), 0666) or $fail = 1;
                } else {
		    open FLAG, "<$lockfile" or $fail = 1;
		}
		if (!$fail) {
			read FLAG, $buff, 256;
				close FLAG;
			dbgprint "lockfile contents: $buff\n";
			my ($pid) = split(" ", $buff);
			my $pid_found;
            if ($CFG::NT) {
                my $exists = DoCmd("$CFG::pid_exists $pid");
                if ($exists =~ /PID not found/) {
                    $pid_found = 0;
                } else {
                    $pid_found = 1;
                }
            } else {
                my $exists = DoCmd("CFG::ps -p $pid");
                if ($?) {
                    $pid_found = 0;
                } else {
                    $pid_found = 1;
                }
            }

			if (!$pid_found) {
			dbgprint "locking PID $pid has exited, lockfile is stale. Removing...\n";
			# kill the stale lockfile, then try again.
			if (unlink $lockfile) {
				dbgprint "removed lock, attempt GetMutexLock() again...\n";
				return GetMutexLock($lockfile);
			} else {
						print STDERR "Unable to remove stale lockfile '$lockfile'\n";
						return 0;
					}
			}
		} else {
	        dbgprint "opening existing lockfile $lockfile failed\n";
	    }
    } else {
        print FLAG "$$ " . MakeTimestamp();
        close FLAG;
        dbgprint "successful.\n";
        return 1;
    }

}

sub ReleaseMutex {
    if ($CFG::disable_locking) {
        return 1;
    }
    my $lockfile = shift;
    if (!(-e $lockfile)) {
        dbgprint "mutex lockfile $lockfile previously removed\n";
        return 1;
    }
    if (unlink $lockfile) {
        dbgprint "mutex lockfile removed: $lockfile\n";
        return 1;
    } else {
        qprint "WARNING: unable to remove lockfile $lockfile!\n";
        return 0;
    }
}

sub GetDenseOid {
    my $oid = GetOid (@_);
    $oid  = OidToDenseOid($oid);

    return $oid;
}

sub OidToDenseOid {
    my $oid = shift;
    $oid  =~ tr/:.//d;
    return $oid;
}

1;
