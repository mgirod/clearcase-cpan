# Introduction #

In the current version, commands prompting the user, while in ipc mode, work in a few cases on unix, but fail most of the time, resulting in hanging behaviour.

This is not very important for scripts, but is a pain in wrappers. Fortunately, wrappers may handle some of the issues before invoking ClearCase::Argv.
The issues which cannot reasonably be handled concerned specific interactions, such as in `merge`. Generic interactions are those related to comments, or forcing the behaviour for instance: these may be _preempted_ by a wrapper, i.e. fully handled by the wrapper, which inserts flags to avoid them in the `cleartool` behaviour proper.

Several distinct problems interfere with the code:
  * a ClearCase bug with `setview` under `cleartool -status`
  * differences in behaviour between unix and Windows (Note that they affect CtCmd as well)
  * on Windows, `cleartool` doesn't use `pty`s but `CONIN$` and `CONOUT$` devices

# Details #
  * The MGi wrapper attempts now to preempt the user interactions, and thus to avoid the problem. It does this for now only for comments, and with `mklbtype`. It will now be extended to the other commands, and to the other interactions (see below: no real fix was found at the ClearCase::Argv level; however, wrappers may handle them all).
  * There will however remain for a later phase niceties such as saving comments from co to ci, or as a default to later commands; or maybe a '!' termination instead of '.', in order to break interactive looping and use the same values for the remainder of the arguments.

Otherwise, at the level of infrastructure:
  * A work-around is in place for the [bug](http://www-128.ibm.com/developerworks/support/rational/rfe/execute?use_case=searchChangeRequests&PROD_ID=0&COMP_ID=0&RESP_ID=0&CREATED_BY=314) (RFE 423, acknowledged as defect RATLC01064211, then closed as a user error, maybe reopened, but in an unclear state anyway) in `cleartool -status`
  * The work-around works on unix for certain prompts, but as far as I know, always fails on Windows
> 'setview' requests are detected, and serviced via a second process started with (_foo_ being the view):
```
  cleartool setview -exec "cleartool -status" foo
```
  * The regexp for user prompts is too restrictive, and doesn't support e.g.:
```
  Checkout comments for "Changes":
  Save private copy of "Changes"?  [yes] 
```
  * In the latter example, the user input is limited to one line, whereas the code expects a single dot as for multiline comments.
  * In any case, there is something which looks like a flushing problem (although setting the pipes to autoflush doesn't solve it): some prompts from cleartool are not readable until the background process is killed (for lack of an other way). This was experienced on unix, but may apply to Windows as well.