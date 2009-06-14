#!/usr/bin/perl -w

use strict;
use Net::SSH::Perl;
use ClearCase::VobPathConv;

{
  my $cmd = '/usr/bin/locklbtype';
  my $ssh;
  sub ssh() {
    my $host = 'my.unix.host';
    $ssh = Net::SSH::Perl->new($host) unless $ssh;
    $ssh->login(0, 0, 1);
    return $ssh;
  }
  sub funlocklt($$) {
    my ($lt, $vob) = @_;
    $vob = winpath2ux($vob);
    my($out, $err, $ret) = ssh()->cmd("$cmd --unlock --vob $vob --lbtype $lt");
    return $out;
  }
  sub flocklt($$;$) {
    my ($lt, $vob, $nusers) = @_;
    $vob = winpath2ux($vob);
    $cmd .= " --vob $vob";
    $cmd .= " --nusers $nusers" if $nusers;
    my($out, $err, $ret) = ssh()->cmd("$cmd --lbtype $lt");
    return $out;
  }
}

1;
