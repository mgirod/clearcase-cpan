# Status #

Support is implemented, in both cases, and working.

# Details #

## Branch Types ##

With branch types, the driving semantic is to _archive_ the existing type _away_, so that existing config specs will stop selecting the branches after they have been rolled out (published), and will select the public labels (baseline) instead.

This is fine, since although a new type is created, which is used nowhere, this situation is not problematic: the type will get used _implicitly_ at the next checkout.

## Label Types ##

With label types, it is not appropriate to create a type and not to use it: the driving part of the semantics is _mklbtype_ and the reason is that labels are _applied_ explicitly.

In fact, archiving a label type should arguably not happen in conjunction with mklbtype. It is also available as a separate command: _ct archive_. However, the solution taken is to rename away the label type just created, so that one may retain the pointer to the previous archived type. This type will be retrieved while creating the next family type: this way, it will come already linked.

Of course, the type should not be used (applied) during the sleeping phase. We lock it.