# Introduction #

We know of quoting issues in ClearCase::Argv but not in ipc mode.
[issue 13](http://code.google.com/p/clearcase-cpan/issues/detail?id=13) was finally considered 'not a bug'.

This was the state until [issue 28](http://code.google.com/p/clearcase-cpan/issues/detail?can=1&q=28&colspec=ID%20Type%20Status%20Priority%20Milestone%20Owner%20Summary&id=28).
This one reveals a backwards compatibility problem.
The fix proposed in [r247](http://code.google.com/p/clearcase-cpan/source/detail?r=247) brings however a new batch of backwards incompatibilities, maybe more recent than the ones it aims at solving.

Some usage patterns must be identified and discouraged.
One example is the following, unfortunately matching the command line:
```
$ct->argv(qw(des -fmt "%[owner]p\n" vob:.))->system
```

The problem is to delegate the parsing to the `qw` function, which is not part of ClearCase::Argv. In old and new versions of ClearCase::Argv, the `"%[owner]p\n"` token is itself quoted in single quotes, with unwanted results.
These results are of course desirable if the situation is the following:
```
$ct->argv(qw(mkattr -nc FOO "foo"))->system
```

This wouldn't happen in the intermediate versions, but the weakness of this idiom shows up if one adds a space inside the format, since `qw` will use it for its parsing.

# Details #

The main mechanism to deal with quoting issue is via the quote member function, in Argv.pm, and overloaded in ClearCase::Argv.
It is called for now from Argv::system readpipe, qx, under testing the autoquote variable.

Now, those functions are not called from the ClearCase::Argv overloaded members, in ipc or ctcmd mode.