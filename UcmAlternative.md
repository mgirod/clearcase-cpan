# Introduction #

[UCM is bad](http://www.cmwiki.com/DontUseUcm). It fills however a need. Can we have our cake and eat it?

# Details #

Problems with UCM:
  * Inflexible: components, p- and admin- vobs, mastership
  * GUI and opacity: anti-SCM
  * Weight and poor performance, coupling (access restrictions, synchronization, waiting)
  * [Obsolete](http://www.cmwiki.com/UcmObsolete): anti-ClearCase

The pros, the needed functionality
  * Baselines, activities, change sets: higher level vision
  * Incremental labeling

The proposed solution:
  * Implicit branching: the rule, no exception
  * [Publish in-place](http://www.cmwiki.com/NoMergeDelivery): promote
  * Fully roll-back able, and ready to roll-forward again
  * Stability, and avoidance of duplication