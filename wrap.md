# ClearCase::Wrapper #

I got a fair batch of valuable contributions ([r546](https://code.google.com/p/clearcase-cpan/source/detail?r=546)) which I failed to incorporate all.
There was a problem (including backwards compatibility) with one aspect.
I believe I could reimplement the functionality.
However, I dropped for the time being the rest of the submission.
There remains valuable ideas:
  * collect functions provided by one module but overshadowed by another, instead of ignoring them
  * allow the user to specify a list of excluded functions
  * allow to drop back to an overshadowed function, instead of only to the native cleartool one (some kind of inheritance).

# Details #

The base changes concerned the control of auto splitting the functions.
The main advantage (which I implemented differently) was to restore the effect of _warnings_ and _strict_, and to ease the use of the Perl debugger.