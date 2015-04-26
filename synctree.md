# ClearCase::SyncTree #

Some recent significant novelties, with some remaining issues ([r578](https://code.google.com/p/clearcase-cpan/source/detail?r=578))

# Reuse, and Version Reuse #

_Shopping for versions_ as _shopping for DOs_

Use of -/ipc=1 flag: benefit for `checkin -from` commands, which must be run one by one, and are used to preserve config records. Note also that they could be more efficient than copying first and doing a local mkelem, which incurs thus two file copies instead of one.

# Issues #

  * ClearCase symbolic links are not recognized as such on Windows and Cygwin, by perl -l operator and readlink function: support missing for the source base, which may be needed if using synctree for staging purposes
  * The source files are copied too early, so that they are overwritten or thrown away in several cases (`checkin -from`, version reuse, resolving symlinks in the vob)
  * The `checkin -from` option is maybe not respected in all scenarios