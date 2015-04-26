# Background #

UNIX had been developed at AT&T labs, in relation and opposition to academia, at Berkeley and elsewhere.
Two companies had developed to make profit by using it as the operating system of the workstations they developed: Sun and Apollo. These were times of bold innovation, which led to divergence.
Apollo/Domain, in spite or because of its originality, became too different.
Apollo was bought by HP.

This was the opportunity of a life-time for the team who had been developing DSEE, the Domain Software Engineering Environment. They ported it on _standard_ UNIX (in practice, Solaris and HP-UX), and created the company Atria.

# Bold Originality #

ClearCase was a breakthrough. Its distinctive feature was a virtual file system, and this one's application, _Workspace Management_, a conceptual novelty. Management was made available where and when it was needed, at the developer's and during the development, before stable versions where achieved. In fact, the artifacts in direct need for management where not _versions_ at all (in the narrow sense of traditional CM: source items under version _control_); they were derived objects: the products of builds, the most volatile and most valuable artifacts, the ones eventually to be delivered.

The main tools were _dynamic views_ and _clearmake_.

# Fear of Repeating Errors #

With UCM, we must admit that a 180 degree turn took place.
Dynamic views and clearmake are just options, seldom in favour, and put in roles in which they cannot express their power. Almost forgotten oddities.

ClearCase has in fact joined back the mainstream of plain old CM, following paths pioneered by other tools.
We may guess that this was the result of an internal struggle at Rational, in favour of market (mostly Windows) driven products, and against pioneering innovation.
The fate of Apollo had maybe its share in this sad strategic decision.