# Introduction #

This is a module by David Boyce,
published and documented in [CPAN](http://search.cpan.org/perldoc?ClearCase::Argv),
currently there as [1.51](http://search.cpan.org/~dsb/ClearCase-Argv-1.51/Argv.pm).

This site aims only at coordinating contributed work on this module,
with a bias toward using it within and as a basis for some other modules
mentioned here.

# Details #

  * There are known problems and limitations (see issues)
    * [interactive comments](CavComments.md)
    * test cases: some cases provided in an 'r' sub-directory. Run with `perl -Mblib`
    * [quoting](Quote.md)
    * ccreport
    * multitool

# General #

ClearCase::Argv offers a consistent interface over 4 different possible sub-layers:
  * a _fork_ model, using the Argv module
  * an _in-process_ model, using the IBM/Rational [ClearCase::CtCmd](http://search.cpan.org/search?query=ClearCase%3A%3ACtCmd&mode=all) module (and possibly in some future, the [ClearCase::MtCmd](http://search.cpan.org/search?query=ClearCase%3A%3AMtCmd&mode=all) one), or,
    * on Windows, the CAL interface
  * a _co-process_ model, using the _ipc_ mode (not bound anymore to the [IPC::ClearTool](http://search.cpan.org/search?query=IPC%3A%3AClearTool&mode=all) module, although using it if found installed.)

Not completely orthogonal to the former, there is obviously the platform dichotomy between Unix and Windows.
Furthermore, the diversity is made more complex by the dynamic use (tested with _eval_) of  various possible _cloning_ techniques, depending on the installed modules.

The goal of ClearCase::Argv is thus to hide this diversity, and to offer, as far as possible, script portability over this multiplicity.

Now, all these options are not quite equal.
Obviously, one main interest (beyond mere portability, valuable in itself) of using a module to access ClearCase, is to boost efficiency by avoiding the initialisation and finalisation of a _cleartool_ (or _multitool_) process for every invocation.

This makes the fork model _secondary_.
The _co-process_ model suffers an other important drawback: the ClearCase::CtCmd module must be recompiled with every ClearCase reinstallation, which is often considered a major burden (with common support models within large organisations, but also especially on Windows, because of the requirement on a build environment), and prevents sharing it to multiple clients, if their configuration is not synchronised.

These considerations orient the work towards ensuring that the ipc mode works optimally, i.e. reusing for successive commands the same background process, unless otherwise required (e.g. for setting different views in different contexts); and in the same way as with the other models. The other models serve in a way to define the standard for the ipc model.