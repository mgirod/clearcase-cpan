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

# registry definitions
$RGY::cc_regy = "Software\\Atria\\ClearCase\\CurrentVersion";
$RGY::ms_st_regy = $RGY::cc_regy . "\\MultiSite\\StorageClass";

sub dbgprint {
    return if (! $CFG::verbose);
    print @_;
    print STDERR @_ if $CFG::stderr_msgs;
}

sub print2 {
    # print to both STDOUT (if qprint allows it) and STDERR
    my $msg = join("", @_);
    qprint $msg;
    print STDERR $msg;
}

sub qprint {
    # print based on 'quiet mode' - if > 1, suppress all; if == 1 only messages
    # containing "WARNING" or "ERROR"; if < 1 just print it.
    return if ($CFG::quietmode > 1);
    my $buff = join('', @_);
    if ($CFG::quietmode == 1) {
        if ($buff =~ m/ERROR/ || $buff =~ m/WARNING/) {
            print STDOUT $buff;
        }
    } else {
        print STDOUT $buff;
	}

    return;
}

sub ForceUntranslatedOutput {
    # save, then delete NLSPATH/LANG EVs so that multitool output (that we
    # parse) is in English/ASCII instead of localized text & char set.
    # Note that for UNIX, NLSPATH must be NULL; for Windows it must be be
    # set to a valid drive directory. "C:\" should be valid on all
    # Windows systems.
    $CFG::save_nls = $ENV{NLSPATH};
    $CFG::save_lang = $ENV{LANG};
    if ($CFG::NT) {
        $ENV{NLSPATH} = "C:$CFG::FS";  # "C:\"
    } else {
        delete $ENV{NLSPATH};
    }
    delete $ENV{LANG};
}

sub RestoreLocalOutput {
    # restore LANG/NLSPATH EVs to previous settings
    $ENV{NLSPATH} = $CFG::save_nls;
    $ENV{LANG} = $CFG::save_lang;
    undef $CFG::save_nls;
    undef $CFG::save_lang;
}

sub DoSystemNoTranslate {
    # run Do_System() while forcing the output to be unlocalized; use when the
    # script must interpret the text messages emitted by the command.
    my $cmd = shift;

    ForceUntranslatedOutput();

    # do the command; save output and return value.
    my $res = Do_System(@_);

    RestoreLocalOutput();

    return $res;
}
    
sub Do_System {
    # run perl's 'system' command, with wrapper to allow print of commands,
    # etc. when desired.
    my ($cmd, $called_from, $called_at) = @_;
    
    print "Do_System: $cmd\n" if ($CFG::verbose || $CFG::traceflg);

    my $res = system($cmd);

    dbgprint "Do_System result: $res ($called_from:$called_at)\n";

    return $res;
}

sub DoCmd {
    # run a shell command, emulating back-ticks. In some versions
    # of perl (on NT), the backtick method of executing a shell command may cause
    # problems with losing STDERR output in subsequent uses of the perl 'system'
    # function.
    # This function gives back the command's STDOUT as its value, and sets $? to
    # the return value from the system call.
    my $cmd = shift;
    dbgprint "entering DoCmd($cmd)\n";
    my $tmpfile = GetTmpdir() . "${CFG::FS}ms_out_$$.tmp";
    my $retval = system("$cmd " . ">$tmpfile");
    dbgprint "in DoCmd() return val is: $retval\n";
    open CMD, "< $tmpfile";
    my @out = <CMD>;
    close CMD;
    unlink $tmpfile;
    $? = $retval;
    return join('',@out);
}

sub DoCmdNoTranslate {
    # run DoCmd() while forcing the output to be unlocalized; use when the
    # script must interpret the text messages emitted by the command.
    my $cmd = shift;

    ForceUntranslatedOutput();

    # do the command; save output and return value.
    my $output = DoCmd($cmd);
    my $retval = $?;

    RestoreLocalOutput();

    $? = $retval;
    return $output;
}
    
sub GetReplicaHost {
    my ($rep_sel) = @_;

    my $cmd = "$CFG::ClearTool describe -fmt \"%[replica_host]p\" ${rep_sel}";
    my $rep_host = DoCmdNoTranslate ($cmd);

    return chomp($rep_host);
}

sub MakeTimestamp {
    # make a timestamp string
    #
	my ($sec,$min,$hour,$mday,$mon,$year,undef,undef,undef) = gmtime(time);
    
	#
	# gmtime returns a month number from 0 - 11 so we add 1,
	# also returns 'years since 1900'; so 'mod' to get 2 digits
	#
	$year %= 100;  # gmtime returns 'years since 1900'; mod to 2 digits
   return sprintf("%02d%02d%02d-%02d%02d%02d", $year, ++$mon, 
		           $mday, $hour, $min, $sec) . "Z";
}

sub GetLocalHost {
    my $localhost;
    if ($CFG::UNIX) {
        $localhost = DoCmd("uname -n");
    }

    # for Windows, or if "uname -n" failed, try "hostname" instead
    if (!$localhost) {
        $localhost = DoCmd("hostname");
    }
    dbgprint "local host name: $localhost\n";
    chomp $localhost;
    return $localhost;
}

sub GetLocalVobs {
    my $localhost = GetLocalHost();
    my @localvobs;

    my $hostnames = "";
    if (-f $CFG::alternate_hostsfile && -s $CFG::alternate_hostsfile) {
        my $cmd = "cat $CFG::alternate_hostsfile";
        $hostnames = DoCmd($cmd);
        chomp($hostnames);
    }
    # add a newline in case file doesn't end with one
    $hostnames .= ("\n" . $localhost);

    my @hostarray = CleanupList(split("\n", $hostnames));
    my $host;

    if ($CFG::UNIX) {
        my $vobs = "";
        foreach $host(@hostarray) {
            my $cmd = "$CFG::ClearTool lsvob -short -host $host";
            $vobs = $vobs . DoCmd($cmd); 
        }
        @localvobs = split("\n", $vobs);
    } else {
        # NT: parse the output of lsvob -long, rather than using lsvob -host.
        # Improves performance for sites with lots of VOBs.
        my $cmd = "$CFG::ClearTool lsvob -long";

        my @all_vobs = split('\n', DoCmdNoTranslate($cmd));

        my $vobtag;
        my $server_host;
        foreach my $lsvob_line (@all_vobs ){
            if ($lsvob_line =~ m/^Tag:\s+(\S+)\s*/) {
                $vobtag = $1;
                undef $server_host;
                next;
            }
            if (($lsvob_line =~ m/\s+Server host:\s+(\S+)\s*/) && $vobtag) {
                $server_host = $1;
                if ($server_host =~ m/^([^\s\.]+)\..*/) {
                    # strip off any domain info (i.e. myhost.domain.com) -
                    # want only "myhost" for the comparison with host names 
                    # stored in the hostarray below.
                    $server_host = $1;
                }

                foreach $host(@hostarray) {
                    if (lc($server_host) eq lc($host)) { # case insensitive
                        push @localvobs, $vobtag;
                        undef $vobtag;
                        undef $server_host;
                    }
                }
            }
        }
    }

    dbgprint "localvobs (before cleanup): @localvobs\n";
    return CleanupList(@localvobs);
}

sub DirOK {
    # verify a directory spec is not NULL, exists, and is writeable
    my $dir = shift;
    return (length($dir) > 0 && -d $dir && -w $dir) ? 1 : 0;
}

sub GetTmpdir {
    # check reasonable places to find a writeable temp directory. If 
    # $CFG::workdir has been set (i.e. -workdir command line option)
    # then override everything and return that as long as it's usable.

    dbgprint "in GetTmpdir() EVs:\n    TMP: " . $ENV{TMP} . "\n" .
		"    TEMP: " . $ENV{TEMP} . "\n" . 
        "    TMPDIR: " . $ENV{TMPDIR} . "\n";
    
    my @dirlist = ();
    
    if ($CFG::NT) {
        @dirlist = ($ENV{TMP}, $ENV{TEMP}, $ENV{TMPDIR}, "c:\\tmp",
					"c:\\temp", "d:\\tmp", "d:\\temp");
        
    } else {   # UNIX
        @dirlist = ($ENV{TMPDIR}, "/var/tmp", "/usr/tmp", "/tmp");
    }
    
    foreach my $dir (@dirlist) {
        return $dir if DirOK($dir);
    }
    
    # if all others failed, try "."
    return ".";
}

sub GetSiblings {
    my $vob = shift;
    dbgprint "GetSiblings() for vob: $vob\n";
    my $sibs = DoCmd("$CFG::MultiTool lsreplica -short -siblings -invob $vob");
    my @siblings = split(" ", $sibs);
}

sub ConsLogFileName {
    # construct a log file name using current date and time; pass in base name
    my $basename = shift;
    if (!$basename) {
        $basename = 'sync';
    }
    return "${basename}-" . MakeTimestamp() . "-$$" . '_log';
}

sub RenameGZFileName {
    # If needed, construct a unique "gz" name for a compressed packet,
	# using the current date and time, and the PID; pass in the existing 
	# basename and suffix (may be "gz", "GZ", "GZ_1", "GZ8", etc.).
	# Return the new filename and result of the rename operation. Note 
	# that result == 0 indicates FAILURE!
    my $basename = shift;
	my $ext = shift;
	my $oldname = "$basename" . "\.$ext";
	my $newname;
	my $result = 0;

	if ($ext eq "gz") {
		# Nothing to do
		$result = 1;
		return ($oldname, $result);
	} elsif ($ext eq "GZ") { 
		# Turn the uppercase "GZ" into lowercase
		$newname = "$basename" . "\.gz";
	} else {
		# Capture the extension as part of the new name
		$newname = "$basename" . "_$ext" . MakeTimestamp() . "-$$" . "\.gz";
	}
	# Now rename the file
	$result = rename($oldname, $newname);
	if ($result == 0) {
		# The rename failed!
		qprint "Unable to rename packet $datafile to $name_gz: $!\n";
	}
	return ($newname, $result);
}

#
# Make sure the log directory exists.  Try to create it if necessary, or
# find/create generic logdir. Return what we come up with.
#
sub ValidateLogDir {
    my $logdir = shift;

    if (!$logdir) {     # if no logdir passed, use default for the OS
        $logdir = $CFG::logdir;
    }

    if (!(-d $logdir)) {
        warn "Could not find logdir ($logdir); attempting to create it.\n";
        mkdir $logdir, 0777;
        if ( $! != 0 ) {
            print STDERR "ERROR: unable to create log directory $logdir\n";
            die "ERROR: can't create log directory: $logdir\n";
        }
    }

    if (!(-w $logdir)) {
        print STDERR "ERROR: log directory not writeable: $logdir\n";
        die "ERROR: log directory not writeable: $logdir\n";
    }

    return $logdir;
}

sub GetReplicas {
    # split a list of replicas into VOB and replica-list
    # valid incoming format is: "rep1@\myvob", "rep1@\myvob,rep2,rep3", or
    # "\myvob" (with no reps specified; implies ALL siblings).
    my $replist = shift;
    my ($r1, $part2) = split('@', $replist, 2);
    my @reps = ();
    my $vob;
    my @more_reps = ();
    if ($part2) {
        if ($r1) {
            @reps = split(',', $r1);
        }
        ($vob, @more_reps) = split(",", $part2);
        push @reps, @more_reps;
    } else {
        ($vob,@reps) = split(",", $r1);
    }

    # check to see if we got a valid VOBname...
    my $st = Do_System ("$CFG::ClearTool lsvob $vob > $CFG::nulldevice", 
        __FILE__, __LINE__);
    dbgprint "VOB $vob list status: $st\n";
    if ($st) {    # some problem with the VOB, time to fail
        print STDERR "ERROR: Unable to locate VOB named '$vob'.\n";
        $CFG::failcount++;
        return (undef);
    }

    if (!(scalar @reps)) {   # no replicas specified, return all siblings
        @reps = GetSiblings($vob);
    }
    return ($vob,@reps);
}

sub HostsEqual {
    my ($host1, $host2) = @_;

    if ($host1 eq $host2) {
        return 1;
    } else {
        my ($i, $l, $h);
        my $len = length ($host1);
        my $len2 = length($host2);

        if ($len2 > $len) {
            $len = $len2;
        }
        for ($i = 0 ; $i < $len; $i++) {
            $l = substr($host2, $i, 1);
            $h = substr($host1, $i, 1);
            if ($l eq "" && $h eq "." || $h eq "" && $l eq ".") {
                return 1;
            }
            if ($l ne $h ) {
                return 0;
            }
        }
    }
}

#
# Redirect STDERR to the error log file.
# Given the pathname of the log file directory,
# create a unique log file name, using a given prefix,
# Open the log file with than name, on the STDERR handle
sub RedirectSTDERRtoLog {

    my ($logdir, $basename) = @_;
    my $name;

    $name = $logdir . $CFG::FS . ConsLogFileName($basename);

    dbgprint "logfilename: $name\n";

    close STDERR;

    if ($CFG::NT) {
        open(STDERR, "> $name");
    } else {
		no strict 'subs'; # quiet ccperl (5.001 1m) on Windows
		my $st = sysopen
		   (TMP, $name, Fcntl::O_RDWR() | Fcntl::O_CREAT() |
		   Fcntl::O_EXCL(), 0755);
        if (!$st) { 
			if (-e $name) {
				# print to STDOUT because STDERR is closed at this point
				print STDOUT "Error: Log file $name already exists.\n"; 
			}
			# print to STDOUT because STDERR is closed at this point
            print STDOUT "Could not redirect STDERR (couldn't open log file): $!";
			exit 1;
        }
        $st = open (STDERR, ">&TMP");
        if (!$st) {
            die "Could not redirect STDERR (couldn't dup STDERR): $!";
        }
    }

    no strict;
    select ((select (STDERR), $|=1)[0]); # flush after every print
    use strict;

    return $name;
}

sub GetOid {
    my ($rep_sel) = @_;
    my $fmt_qq = qq/"%On"/;
    my $cmd = "$CFG::ClearTool describe -fmt $fmt_qq $rep_sel";
    dbgprint "describe cmd: $cmd\n";
    my $rep_oid = DoCmd ($cmd);
    chomp $rep_oid;

    return $rep_oid;

}

# Given a single array of strings as an argument, return a new array with
# no duplicate elements. Also remove any zero-length and all-whitespace
# strings. returned array is alpha-sorted.
#
# Example: 
#     @letters = ("c", "      ", "b", "b", "", "a");
#     @unique_letters = CleanupList(@letters);
#
#     @unique_letters now contains ("a", "b", "c") in alpha sorted order.
#
sub CleanupList {
	my %returnhash = ();

	foreach my $entry(@_) {
        if (length($entry) > 0 && !($entry =~ m/^\s+$/)) { 
		    $returnhash{$entry} = 1;
	    }
	}
	return (sort keys %returnhash);
}

#
# Log errors in the appropriate files: event log on NT, error_log on UNIX
#
sub LogError { 

    my ($Script, $Name) = @_;

    my $cc_error_log = "/var/adm/rational/clearcase/log/error_log";
    my $UNIX_msg = "flush_syncmgr_cache: ERROR: See $Name for details.\n";

    if ($CFG::NT) { # only do event log for NT
	my $cmd ="$CFG::imsglog $Script evtlog \"$Name\"";
	my $res = system($cmd);
    } else { #on UNIX write to ClearCase error_log
	WriteLog ($cc_error_log, $UNIX_msg);
    }
}

#
# Write to the specified error logfile.
#
sub WriteLog {

    my ($logname, $String) = @_;

    open(LOG, ">> $logname");
    print LOG $String;

}

#
# Use TRACE_VERBOSITY to control output to STDOUT
#
sub traceprint {
    my ($trace_string, $vlevel) = @_;

    if ( $vlevel <= $verboseness) {
	print STDOUT $trace_string;
    }
}

#
# Make sure the log directory exists.  Try to create it if necessary, or
# find/create generic logdir. 
#
sub CheckLogDir {

    my ($Dir) = @_;
    my $status = 0;

    if (!(-d $Dir)) {
	print STDOUT "Could not find logdir ($Dir); attempting to create it.\n";
	mkdir $Dir, 0755;
	if ( $! == 0 ) {
		# Couldn't create it. Return log_status 1.
		$status = 1;
	}
     }

     if (!(-w $Dir)) { 
	# Not writeable. Return log_status 2
	$status = 2;
     }

    return $status;
}

1;
