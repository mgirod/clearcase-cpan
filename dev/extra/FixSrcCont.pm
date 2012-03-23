package ClearCase::FixSrcCont;

use warnings;
use strict;
use File::Basename;
use ClearCase::Argv;

our $VERSION = '0.01';
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(add2fix);

my $ct = new ClearCase::Argv({ipc=>1, autochomp=>1});
my $broker = $ENV{FSCBROKER};
my $fxsc = '/vobs/atcctest/ClearCase-Wrapper-MGi/extra/fixsrccnt';
my %tofix;
my $user = getpwuid($<) or die "Failed to get uid: $!";

sub add2fix {
  return unless $broker;
  for (@_) {
    my $ver = $_; #version from which just branched off/checked out
    my ($oidprv, $ele) = $ct->des([qw(-fmt %On\n%En\n)], $ver)->qx;
    my $v0 = $ct->des([qw(-fmt %En@@%PVn)], $ele)->qx; #branch 0, checkedout
    my $oid0 = $ct->des([qw(-fmt %On)], $v0)->qx;
    $v0 =~ s%/0$%%;		#branch
    my $oidbr = $ct->des([qw(-fmt %On)], $v0)->qx;
    my ($dir) = grep s/^source cont="(.*)"$/$1/, $ct->dump($v0)->qx;
    $dir = dirname($dir);
    my $owner = $ct->des([qw(-fmt %[owner]p)], "vob:$ele")->qx;
    $owner =~ s%^.*/%%;
    push @{$tofix{$owner}}, join '@', $dir, $oid0, $oidbr, $oidprv;
  }
}

sub runfix {
  return unless $broker;
  add2fix(@_);
  for (keys %tofix) {
    my $arg = join '@@', @{$tofix{$_}};
    if (/^\Q$user\E$/) {
      system($fxsc, $arg);
    } else {
      system($broker, "$_:$arg");
    }
  }
}

1;
