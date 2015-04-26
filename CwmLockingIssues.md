# Introduction #

We consider two kinds of type _sets_:
  1. label type _families_, with one floating type and a chain of fixed, incremental types,
  1. chains of successive (_archived_) branch or label types.
Note that floating types of a family may be archived, so that successive archived types are chained.

In any case, different types in the set are _linked_ with
hyperlinks of two types: _EqInc_ or _PrevInc_.

Locking/unlocking one type may in fact imply wishing to modify
these hyperlinks, i.e. affect **two** types.

This brings a problem if the different types are not owned by the
same account.

The current implementation differs slightly from the intention
described below (with respect to preserving timestamps, which wasn't implemented).

# Details #

We consider two kinds of locking:
  1. _logical_ (or high-level) locking
  1. _physical_ (or low-level, _silent_) locking
The first case concerns the _floating_ label of an incremental
_family_. Unlocking it, typically in order to apply or to move it,
implies unlocking the _equivalent_ fixed label in the same time,
because applying the one implies applying the other.

Conversely, incrementing the type (`mklbtype -inc`), implies
making changes to the _previous_ type as well, in order to update
the hyperlinks, so that locking the _family_ should in fact lock
**three** different types.

The problem is what to do when all the types are not owned by the
same account.
ClearCase offers no direct support for _group locking_.
This was reported as a [request for enhancement (ID: 1025)](http://www.ibm.com/developerworks/rfe/execute?use_case=viewRfe&CR_ID=1025).

Waiting for a solution to this problem, we may use various
techniques, but all of them have to be set up in the site
configuration. The simplest involves _sudo_ (on unix).

The one sketched in the _extra_ sub-directory uses _ssh_ (which
makes it accessible from cygwin as well).

Both use an indirection, either via the user `~/.clearcase_profile.pl`
file, or via a module required more locally, and specified via an environment variable: **FORCELOCK**. We have to consider two levels:
  * unix
  * Windows
In unix, the problem is to run the `unlock` command as vob owner
of the vob concerned. Since there may be several such accounts, a pure
suid based solution would be impractical. `sudo` offers a
simpler solution by allowing groups of users to access specific
commands using one of several accounts.
If `sudo` is not available, it becomes cumbersome to maintain
such a mapping. The solution proposed here involves authorizing one
dedicated account to access cleartool commands as any vob owner via
`ssh`. A single script must thus be suid enabled to this account.

In Windows, we have neither `sudo` nor `suid` protections.
The solution proposed uses once again `ssh` to access the same
`suid` enabled script as previously, on a unix host. It uses a
`perl` `ssh` client, and is thus identical on Windows and on
`cygwin`.

The various scripts offered require local configuration, of the
account and host names. They do also require that the users set up
pairs of private and public keys. Finally they require that at least
one unix client runs a properly configure `ssh` daemon.

A last problem concerns the tag of the vobs in the unix and Windows
regions. A module is provided to offer the conversion in a generic
way, requiring no special configuration.

It feels difficult to fully decouple the logical
from the physical locking.
One way could be via an `-unlock` option to `mklbtype -inc`.
Otherwise, one doesn't know at `unlock` time what is the purpose
of this operation, i.e. whether unlocking the current fixed type is
logical or physical.

Ideally, implicit locking should preserve the timestamps of locks, since the unlocking is only transient, for the purpose of updating hyperlinks, and immediately followed with a new (identical) lock.