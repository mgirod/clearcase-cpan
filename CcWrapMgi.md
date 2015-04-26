# Introduction #

This wrapper is distributed via CPAN, and was documented [there](http://search.cpan.org/perldoc?ClearCase::Wrapper::MGi)
([sketched in the cm wiki](http://www.cmcrossroads.com/cgi-bin/cmwiki/view/CM/IncrementalTypesImp)).
We'll however gradually [develop these](UcmAlternative.md) here.
It builds upon ClearCase::Wrapper and thus upon [ClearCase::Argv](ClearCaseArgv.md).
It aims at supporting various novel usage patterns of (base) ClearCase.

# Details #

The code on this site is developed in a _dev_ branch.
  * I do not test _ctcmd_ on Unix anymore for the time being. I currently use mostly cygwin, and the port has delayed my main work.
  * My intention was to develop a regression testing suite, which I started to do on ClearCase::SyncTree. I withdraw from my preliminary implementation, meaning that I have nothing solid in either package, which obviously is weak. A proper regression testion suite would surely help me...
  * I also acknowledge minor deficiencies in the support for the branch-no-merge strategy:
    * missing support for graphical `lsgen` (based on Tk)
  * No major [known bugs](http://code.google.com/p/clearcase-cpan/issues/list) now, but:
    * ipc is better supported in ClearCase::Argv (it is even the **only** mode supported under cygwin). The mode is forced in many functions and `ctcmd` may still be preferred for some (which is wrong). It will always succeed on Windows, either on top of ClearCase::CtCmd if installed, or on top of CAL. I originally missed the existence of support in ClearCase::Wrapper for flags such as `-/ipc=1`, and the possibility to set it in the `.clearcase_profile.pl` file.
    * [The source containers get bloated](TypeMgr.md)
  * Intentions:
    * support for incremental types, and first for label types, including:
      * consolidation of sparse fixed labels
    * availability of functionality from ClearCase::Wrapper:DSB, such as inclusion of config spec components
  * Mostly done (although now, I'd like to redesign both to use classes, so that it would be easier/shorter to take it in use in new functions: the current technique is too verbose...):
    * [interactive input](CavComments.md): generalize to avoid all hangs
    * [lock/unlock](CwmLockingIssues.md), using site specific methods when not owning the equivalent or previous types.
  * Done:
    * [mklabel](WmgMkLabel.md) -con, and mklbtype -con
    * `setdepth` recursion was shortened (for performance and scalability reasons) when a depth is given, e.g. supporting `diff -pred`. The ambiguity with 'depth' (default: the number of levels _displayed_ which is intuitive and follows _lsvtree_) may be lifted by using the `-a/ll` option.
    * support for incremental types:
      * support for [global types](GlobalTypes.md) (using admin vobs or not)
      * folding config specs (automatic generation of the cascading rules)
    * [mkxxtype -arc, and/or archive (br|lb)type:xxx](WmgArc.md)
  * Still not final:
    * [rollout/rollback](RollOutBack.md) to rollback versions (faster than through generating view equivalent to previous state)