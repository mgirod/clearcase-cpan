require ClearCase::Argv;
# Argv->dbglevel(1);
ClearCase::Argv->ipc(2) unless $ARGV[0] eq 'setview';

{
  my $forcelock = '/usr/bin/locklbtype';
  sub funlocklt($$) {
    my ($lt, $vob) = @_;
    return system($forcelock, '--unlock', '--vob', $vob, '--lbtype', $lt);
  }
  sub flocklt($$;$) {
    my ($lt, $vob, $nusers) = @_;
    my @fargs = ($forcelock, '--vob', $vob);
    push @fargs, '--nusers', $nusers if $nusers;
    return system(@fargs, '--lbtype', $lt);
  }
}
