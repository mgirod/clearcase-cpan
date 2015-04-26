# ChangeSet #

Despite the UCM _deliver_, the rollout and rollback functionalities make only sense together.

They are implemented by means of convenience wrapping up of pre-existing functionality.

If the changeset being rolled out is well-defined, the one being rolled-back is not easily identified. The offering so far uses `mkview -clone`, which works (until proven otherwise), but is relatively heavy, and non obvious to trace back.

The alternative of recording the changeset potentially to be rolled back is possible in many different ways, but was rejected (at least so far).

# Implementation ideas #

  * rollback is a special form of rollout: it produces a **new** (equivalent fixed) label type, higher than the previous
  * rolling back the rollback offers thus the same data, but a new state
  * the same versions may thus have to be rolled back several times, in different contexts (as part of different rollbacks)
  * alternative options (the last one being the one implemented)
    * create and apply a new label type, e.g FOOb1.01 for FOO\_1.01 (none needed for FOO\_1.00!?)
      * heavy?
      * noise generating?
    * saving the changeset as an attribute
      * using oids of the versions
      * OK for a few, but what if there are 100s
      * probably lighter at rollout, and not cluttering the vtree...
      * maybe less robust (removed elements?)
      * maybe alternative implementation for small numbers (<=10)
    * not saving the changeset at all, but relying upon `mkview -clone -equiv` to compute it as needed. This may be a fallout strategy and win on large numbers. This is the approach implemented for now.
  * there will be some trivial rollbacks (nothing rolled out on top), and more problematic ones
    * for the problematic elements:
      * branch off/checkout
      * subtractive merge
      * alter the merge arrows to skip the removed version from the contributing path
  * the baseline label type used as target should be locked as a result of the rollout. The rollout should thus abort if this one is found unlocked (possible collision of independent rollouts). The problem is that locking (by group members) is currently implemented in a way requiring user configuration, which may be problematic especially on Windows. This behaviour is thus made configurable (possibly on a site basis).
# Bugs/Glitches #
    * rolling out requires mastership of the type one rolls out (from).
    * one can roll out only once, since the type gets archived.
    * the current rollback functionality only applies to the topmost increment(s).