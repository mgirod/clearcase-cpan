# Introduction #

Command supported by [ClearCase::Wrapper::MGi](CcWrapMgi.md): If the first argument is a floating label type, one shall apply the equivalent fixed label type first, only on versions not already bearing the floating, and then move the floating over those versions.

# Details #

In addition, we retain the _-up_ extension from ClearCase::Wrapper::DSB, which allows to apply labels upwards, to versions needed to access the arguments.

  * One problem is what to do in case the first application fails: by default, abort. Note that it may fail only partly
    * One case is this of recursive labeling, in presence of links. In this case, the error reported is that a element was already labeled. The label is not applied a second time (whether on the same or on an other version), but the rest of the elements is labeled. In this case, we ignore the error and move the floating label as well.
    * For this case, I added a _-force_ option...
    * Note that the error may happen with hard links, in which case the versions may differ, so that additional information is needed to decide what to do; but it may also happen with symbolic links, and then, the versions are always the same, so the error is spurious...
  * I check locks on the floating type:
    * The rationale for the lslock check, is that the first label which will be applied is the equivalent fixed one. One needs thus to anticipate a failure to apply the floating...
    * What drives the vob selection to pick the type, is the element (unless the vob of the type is explicitly specified). A type short name is only a string pattern, which may resolve into several distinct types, if elements are given in different vobs.
    * This case anticipates the problems with supporting the -con option.
    * Note that symbolic links are not followed.
    * The solution is to create a hash indexed per vobs, of both elements and equivalent types.
  * The _-con_ option is now supported (consolidation pending)
  * This forces to work on global types. Note that these ones do not necessarily imply using an admin vob: one may use _cptype_ to maintain hyperlinks. In fact, admin vobs are ignored: the types must be created in advance (with mklbtype -con), and the _source_ vob of the type must match this of the config record.
  * One other case is getting the list of versions as a file, or from a pipe, e.g. supporting Audited Objects? Performance may become an issue (running _des -s vob:xxx_ on every argument...)