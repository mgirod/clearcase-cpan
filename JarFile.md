# Introduction #

Jars, like tars, wars, and all kinds of zip archives are seen to ClearCase as binary files.
They are stored in whole versions (compressing them makes little sense, since it is likely to only add a header, and thus produce a file larger than the original, with the added costs of maintaining a cleartext container, and requiring cpu cycles and temp space for the compression/decompression), even when in fact the differences from one version to the next, as can be seen with tools like [jardiff](http://www.osjava.org/jardiff), can be minimal.

What could be an intelligent ClearCase element type manager for jars?

# Details #

  * Jars can be extracted (maybe recursively) and saved as directory trees.
  * The ClearCase::SyncTree package offers an excellent tool to maintain such trees, especially with the BranchOff: root strategy to maximize sharing.
  * These directory trees offer fine grained locality of changes, optimally in text files for which changes can be stored as diffs.
  * The challenge is thus to maintain the source container side as such directories, yet to offer the binary as clear text representation.
  * The novelty is to use ClearCase as a support for itself, to wrap a new layer of ClearCase over a hidden implementation.
  * Next, the cleartext containers ought to be implemented as derived objects.