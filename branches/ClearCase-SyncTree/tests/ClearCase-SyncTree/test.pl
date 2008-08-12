use Test::More tests => 1;
use ClearCase::SyncTree;
use File::Temp qw(tempdir);
use vars qw($vbstg $vwstg $vbtag $vwtag %testdata);
use strict;
use FindBin;
use lib "$FindBin::Bin/blib/script";
require "synctree";

ok (1, 'The SyncTree module was correctly loaded');

if (!`cleartool pwd -h`) {
    print qq(

************************************************************************
ClearCase::SyncTree is only usable when ClearCase is installed.
ClearCase could not be found so testing will not continue. If you
really do have ClearCase installed you may need to add it to PATH.
************************************************************************

);
    exit;
}

use Cwd;
use strict;
use File::Temp qw(tmpnam);
use constant MSWIN => $^O =~ /MSWin32|Windows_NT/i ? 1 : 0;
use Sys::Hostname;
ClearCase::Argv->ctcmd(1) unless ClearCase::Argv->ipc(1);

sub preamble($$) {
    my $ct       = shift;
    my $hostname = shift;
    if (MSWIN) {
	my @shares =
	    grep s/^(\w+) +[A-Z]:\\\w+ +\w.*$/$1/,
	    qx(net share);
	chomp(@shares);
	my $share;
	while (my $s = shift(@shares)) {
	    if ($s =~ /\w+\$$/) {
		next;
	    } else {
		$share = $s;
		last;
	    }
	}
	if ($share and $hostname) {
	    $vbstg = '\\\\' . $hostname . '\\'
		. $share . '\\tmpCCSTtestVOB.vbs';
	    $vwstg = '\\\\' . $hostname . '\\'
		. $share . '\\tmpCCSTtestVIEW.vws';
	}
	$vbtag = q(\tmpCCSTtestVOB);
	$ct->argv('mkvob', '-tag', $vbtag, '-c',
		  q("Temporary vob for ClearCase::SyncTree make test"),
		  $vbstg)->system;
    } else {
	$vbstg = tmpnam();
	$vwstg = tmpnam();
	$vbtag = tmpnam();
	mkdir($vbtag);
	$ct->argv('mkvob', '-tag', $vbtag, '-c',
		  q("Temporary vob for ClearCase::SyncTree make test"),
		  '-host', $hostname, '-hpa', $vbstg, '-gpa', $vbstg,
		  $vbstg)->system;
    }
    if ($?) {
	print qq(

************************************************************************
Failed to create a temporary vob in order to test ClearCase::SyncTree.
This is only a prerequisite for running the test suite, not part of the
test suite itself.
Many things may prevent vob creation: client only installation, non
supported file system, no license available, etc.
************************************************************************

		 );
	exit;
    }
    $ct->argv('mkview', '-tag', $vwtag,
	      '-host', $hostname, '-hpa', $vwstg, '-gpa', $vwstg,
	      $vwstg)->system;
    if ($?) {
	print qq(

************************************************************************
Failed to create a temporary dynamic view in order to test ClearCase::SyncTree.
This is only a prerequisite for running the test suite, not part of the
test suite itself.
Many things may prevent view creation: non supported file system, no 
license available, etc.
************************************************************************

		 );
	$ct->argv('rmvob', '-f', $vbstg)->system;
	exit;
    }

    $ct->argv('mount', $vbtag)->system;
    $ct->argv('startview', $vwtag)->system;
}
sub cleanup($$) {
    my $ct  = shift;
    my $pwd = shift;
    if ($ct->ipc() or $ct->ctcmd()) {
	$ct->stdout(0)->argv('cd', $pwd)->system;
    } else {
	chdir($pwd);
    }
    $ct->argv('endview', $vwtag)->system;
    $ct->argv('umount', $vbtag)->system;
    rmdir($vbtag) unless MSWIN; 
    $ct->argv('rmview', '-tag', $vwtag)->system;
    $ct->argv('rmvob', '-f', $vbstg)->system;
}
sub initialtestdata($$) {
    my $dir = shift;
    my $datref = shift;
    my $cwd = getcwd();
    chdir($dir);
    for my $obj (keys %{ $datref }) {
	if ($datref->{$obj}{type} eq 'd') {
	    mkdir($obj);
	    next;
	};
	if ($datref->{$obj}{type} eq 'f') {
	    open(F1, ">$obj");
	    print(F1 "foo\n");
	    close(F1);
	    if ($datref->{$obj}{attr} eq 'x') {
		chmod(0755, $obj) unless MSWIN;
	    }
	    next;
	}
	if ($datref->{$obj}{type} eq 'l') {
	    link($datref->{$obj}{val}, $obj);
	    next;
	};
    }
    chdir($cwd);
}

my $hostname = hostname();
my $ct = new ClearCase::Argv({autochomp=>1, stdout=>0});
ClearCase::Argv->dbglevel(1);
$vwtag = q(tmpCCSTtestVIEW);
my $pwd = getcwd();
preamble($ct, $hostname);

my $viewpf = '';
if (MSWIN) {
    my ($drv) =
	grep s/^ +([A-Z]:) +\\\\view +ClearCase Dynamic Views$/$1/,
	qx(net use);
    chomp($drv);
    $viewpf = $drv . "\\${vwtag}${vbtag}";
} else {
    $viewpf = "/view/${vwtag}${vbtag}";
}
if ($ct->ipc() or $ct->ctcmd()) {
    $ct->stdout(0)->argv('cd', $viewpf)->system;
} else {
    chdir($viewpf);
}
$ct->argv(q(catcs))->stdout(1)->system;
my $srcdir = tempdir(CLEANUP => 1);
print "Source dir: $srcdir\n";
%testdata = (
	     'dir1'     => { type => 'd', mod => 1,
			 val => {
			     'file' => { type => 'f', mod => 0 }
			 } },
	     'dir2'     => { type => 'd' },
	     'emptydir' => { type => 'd' },
	     'dir with spaces in name mod'    => { type => 'd', mod => 1 },
	     'dir with spaces in name no mod' => { type => 'd' },
	     'textfile2mod'  => { type => 'f', mod => 1 },
	     'textfilenomod' => { type => 'f' },
	     'text file with spaces in name' => { type => 'f' },
	     'emptytextfile' => { type => 'f' },
	     'file.exe' => { type => 'f', attr => 'x' },
	     'link2mod' => { type => 'l', val => 'textfile2mod', mod => 1 },
	     'linknomod' => { type => 'l', val => 'textfile2mod' },
	     'link2spaces' => { type => 'l',
				val => 'text file with spaces in name' },
	     'link with spaces in name' => { type => 'l',
					     val => 'textfile2mod' },
	     
	     );
initialtestdata($srcdir, \%testdata);
Local::Modulino->testrun('-sb', $srcdir, '-db', $viewpf, '-yes', '-ci');

cleanup($ct, $pwd);
