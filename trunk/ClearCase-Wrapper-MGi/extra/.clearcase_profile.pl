require ClearCase::Argv;
# Argv->dbglevel(1);
ClearCase::Argv->ipc(2) unless $ARGV[0] eq 'setview';
