require ClearCase::Argv;
# Argv->dbglevel(1);
ClearCase::Argv->ipc(2) unless exists $ARGV[0]
  and $ARGV[0] =~ /^(setview|(find)?merge)$/;
$ClearCase::Wrapper::MGi::lockbl = 1;
$ENV{FORCELOCK} = 'ClearCase::ForceLock';
