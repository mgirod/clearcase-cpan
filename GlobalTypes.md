# Global types are useful #

Config specs use types only by name.
The names are guaranteed to be unique per vob, but not across vobs.
It results that the config specs are not guaranteed to deal with the _same_ objects (even semantically, considering the intention for their creation): there may easily be accidental matches.
If one wants to have the effect of a config spec rule span across several vobs, one may use _cptype_ to copy the types.
The resulting objects are however distinct.

Types may be _global_ or _ordinary_.
Global types may be linked with _GlobalDefinition_ hyperlinks.
The result is for the _local_ type, source of the _GlobalDefinition_ hyperlink, to accept a role of proxy, delegating its function to the remote homonymous type. This is convenient, as its extends the scope of action of functions such as _lock_ or _rmtype_. It does in effect share the types between several vobs.

# The concept of admin vob, and its shortcomings #

Copying types, and linking them, only concerns existing types.
Without script support, this requires several commands and a good understanding of the tools.

Admin vobs allow to create new copies of types pre-existing in a centralized location. The new copies may even be created as needed, e.g. while applying labels with _mklabel_. At first sight, this is thus a valuable convenience.

The price to pay is unfortunately too high.

Let's first notice that the _GlobalDefinition_ hyperlinks are, as all hyperlinks, directed, and that this directivity is useful and part of the value of global types. Admin vobs form however an acyclic global graph, hence grab this directivity in a generic way, at the vob grain, not at the specific grain of individual types.

This request for consistency is excessive at the site level: it hardly grows bottom-up. Different types will produce conflicting pressures. The worst is however met with MultiSite. The probability of conflicts raises with the number of sites and the decrease of coupling between them. The admin vob graph must be universal, which breaks the fairness among replicas.

## _lstype_ errors or warnings ##

MultiSite replication works at the vob level. This means that the production, shipping, and import of packets for different vobs is decoupled. It makes it unavoidable that cross-vob hyperlinks are now and then in a transient way dangling.

However, missing an admin vob is a global error: it may affect unpredictable commands (applying arbitrary types). The effect cannot be restricted to certain types. This may be detected by running _lstype_ queries. At that point, in the absence of sufficient information about the real impact, the system emits an error which aborts the query.

As we saw above, these conditions are met randomly under normal conditions. Note again that without admin vobs, the scope of the impact is limited to the types concerned: a warning is sufficient, and there is no reason to hold the availability of the _lstype_ command.

The conclusion is that admin vobs result in a global dependency on the state of the replication coupled to the creation of types.

# Global types without admin vobs #

It results of the analysis so far that global types are useful, but admin vobs should be avoided.

Some wrapper support may ease the use of global types:
  * _cptype_ may link the types it copies
  * _archiving_ a family type may insure that the hidden type is created in all the vobs in which the type had been created, so that it may be 'revived' (with a correct value for the next equivalent fixed type) from any of the vobs
  * types created from a config record (non implemented yet) should be global by default
  * types created in an admin vob are global by default (this is from DSB wrapper -- surprisingly not in _cleartool_)