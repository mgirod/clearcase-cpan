# Introduction #

ClearCase is available on Windows, but not on CygWin.
Supporting CygWin in ClearCase::Argv should result in a wrapper offering the illusion of a port of ClearCase onto CygWin.

This support is available now in the 1.43 version on CPAN.

# Details #

The support could be exclusively in ClearCase::Argv, if one could force the use of the `ipc` mode. However, the treatment of interactive dialogs in the `ipc` mode requires specific code. Therefore, it is a tradeoff to offer minimal support for the `fork` model as well, thus in Argv. The bulk of the applications one might want to use via Argv are anyway CygWin, not Windows, applications.
The interest of the port is the usage of cleartool via a wrapper.

The problems to solve were:
  * input paths
  * output paths
  * end-of-line convention in cleartool output
  * the use of Windows I/O devices, not supported via cygwin ptys.

Note that perl can be installed both on Windows and on Cygwin.
In fact, it is available in pre-compiled format for both, for Windows from Active State,
for cygwin ...from Cygwin.
These two installations cannot really be used 'on the other side'. To convince yourself,
try on cygwin (with Perl installed on Windows under C:\Perl):
```
/cygdrive/c/Perl/bin/perl -e 'print "$^O\n"'
MSWin32
```
and on Windows (with cygwin installed in C:\cygwin):
```
C:\cygwin\bin\perl -e "print qq($^O\n)"
cygwin
```
I.e. they both seem to work, but do not detect the real environment under which they were
invoked, but rather enforce the one under which they were built.

I started to work on it in a _cygwin_ branch.
  * In order to deal with `system` or `pipe` (`system` is required for a wrapper), I forced the use of the `ipc` mode, allowing me to post-process the results.
  * Piping commands to cleartool wrappers may work only with the `ipc` model.
  * `lsvob` returns vob paths without any kind of view mapped drive. For now, I accepted this and did not attempt to change it, but I converted the slashes for consistency with the vob specifications for vob objects, such as `'lbtype:FOO@/vobtag'` (accepted as input and returned as output).
  * To try the ClearCase::Wrapper wrapper, you need a version explicitely supporting cygwin (from 1.15), which will force the use of ClearCase::Argv instead of reverting to the underlying cleartool: this will handle `ctx setcs /tmp/cs`.
  * `ctx lsgen ...` and `ctx co` seem to work.