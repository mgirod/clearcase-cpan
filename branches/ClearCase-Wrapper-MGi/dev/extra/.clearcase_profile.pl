require ClearCase::Argv;
# Argv->dbglevel(1);
ClearCase::Argv->ipc(2) unless exists $ARGV[0] and $ARGV[0] eq 'setview';
