# Introduction #

The source containers for elements of type _text file_ get bloated.
Every new branch results in a full copy. This is only a minor nuisance but may eventually result in problems.

Note that this may also have positive consequences, e.g. enhance the performance of version handling (at the expense of wasting disk space, with extra considerations e.g. concerning the position of disk cluster boundaries).

Solving it is possible in principle via a special element type manager.

But this opens a can of worms in the context of poor support for managing user types on ClearCase clients. Can one avoid it?
The answer is **yes**!

# Details #

One might think of branching off an intermediate branch, itself under /main/0. This would however need to be of a shared type (e.g. _m_), and shared branch types are not supported.
In addition, it is likely that this wouldn't help: the base of every new branch would still be empty, and difference from it thus a full copy.

User type managers bring their own quirks: they need to be installed on every client, in a platform suitable way.

Note that the same issue affects some other types than _text files_ e.g. when the source container is shared on a per branch basis.

I analyzed the format of source containers for text file elements [in the cm wiki](http://www.cmcrossroads.com/cgi-bin/cmwiki/view/CM/FormatSrcCnt).

In the case of non-cascading branches off /main/0, the effect observed is twofold:
  * all the branches are marked as '0 0', i.e. spawn from /main/0
  * all the versions 1 start with an insertion record '^I n 1' which is a full copy

## Edit the previous version in the source container at checkout/mkbranch ##
This gives a hope that modifying sligthly this header information might be sufficient!
```
bar> ct ls a
a@@/main/mg-015/1                                        Rule: MG [-mkbranch mg]
bar> cat a

4444
bar> ct dump a | grep cont=
source cont="/cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-b17e438b31a011e1961f00156004455c-f9"
clrtxt cont="/cc/views07/emagiro/bar.vbs/c/cdft/2a/10/b17e438b31a011e1961f00156004455c"
bar> cat /cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-b17e438b31a011e1961f00156004455c-f9
^S db1 2
^V b17e438b.31a011e1.961f.00:15:60:04:45:5c 2 1 4efb93c2
^V bc8e4889.319f11e1.87ca.00:01:84:2b:ec:ee 2 0 4efb9220
^B bc8e4885.319f11e1.87ca.00:01:84:2b:ec:ee 2 1 1
^V fa3e436f.319f11e1.961f.00:15:60:04:45:5c 1 1 4efb90f3
^V 088e43df.31a011e1.8195.00:01:84:2b:ec:ee 1 0 4efb90f3
^B 088e43db.31a011e1.8195.00:01:84:2b:ec:ee 1 0 0
^V 088e43d7.31a011e1.8195.00:01:84:2b:ec:ee 0 0 4efb90f3
^B 088e43d3.31a011e1.8195.00:01:84:2b:ec:ee 0 0 0
^E 088e43cf.31a011e1.8195.00:01:84:2b:ec:ee
^I 1 1 1

^I 2 1 1
4444
bar> ct co -nc a
Created branch "mg" from "a" version "/main/0".
Checked out "a" from version "/main/mg/0".
bar> cd -
/cc/views07/emagiro/bar.vbs/s/sdft/13/32
32> ls -l
total 8
-r--r--r--   1 emagiro  at1          619 Dec 28 22:16 0-a70e556531a111e19d2e0001842becee-f6
32> head 0-a70e556531a111e19d2e0001842becee-f6
^S db1 3
^V a70e5565.31a111e1.9d2e.00:01:84:2b:ec:ee 3 0 4efb9558
^B a70e5561.31a111e1.9d2e.00:01:84:2b:ec:ee 3 0 0
^V b17e438b.31a011e1.961f.00:15:60:04:45:5c 2 1 4efb93c2
^V bc8e4889.319f11e1.87ca.00:01:84:2b:ec:ee 2 0 4efb9220
^B bc8e4885.319f11e1.87ca.00:01:84:2b:ec:ee 2 1 1
^V fa3e436f.319f11e1.961f.00:15:60:04:45:5c 1 1 4efb90f3
^V 088e43df.31a011e1.8195.00:01:84:2b:ec:ee 1 0 4efb90f3
^B 088e43db.31a011e1.8195.00:01:84:2b:ec:ee 1 0 0
^V 088e43d7.31a011e1.8195.00:01:84:2b:ec:ee 0 0 4efb90f3
32> perl -pi -e 's/B a70e5561.31a111e1.9d2e.00:01:84:2b:ec:ee 3 0 0/B a70e5561.31a111e1.9d2e.00:01:84:2b:ec:ee 3 2 1/' 0-a70e556531a111e19d2e0001842becee-f6
32> head -3 0-a70e556531a111e19d2e0001842becee-f6
^S db1 3
^V a70e5565.31a111e1.9d2e.00:01:84:2b:ec:ee 3 0 4efb9558
^B a70e5561.31a111e1.9d2e.00:01:84:2b:ec:ee 3 2 1
32> cd -
/local/scratch/bar
bar> echo 5555 >> a
bar> ct ci -nc a
Checked in "a" version "/main/mg/1".
bar> cat a

4444
5555
bar> ct dump a | grep cont=
source cont="/cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-f78e43a731a111e1961f00156004455c-6j"
clrtxt cont="/cc/views07/emagiro/bar.vbs/c/cdft/37/42/f78e43a731a111e1961f00156004455c"
bar> cat /cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-f78e43a731a111e1961f00156004455c-6j
^S db1 3
^V f78e43a7.31a111e1.961f.00:15:60:04:45:5c 3 1 4efb95e6
^V a70e5565.31a111e1.9d2e.00:01:84:2b:ec:ee 3 0 4efb9558
^B a70e5561.31a111e1.9d2e.00:01:84:2b:ec:ee 3 2 1
^V b17e438b.31a011e1.961f.00:15:60:04:45:5c 2 1 4efb93c2
^V bc8e4889.319f11e1.87ca.00:01:84:2b:ec:ee 2 0 4efb9220
^B bc8e4885.319f11e1.87ca.00:01:84:2b:ec:ee 2 1 1
^V fa3e436f.319f11e1.961f.00:15:60:04:45:5c 1 1 4efb90f3
^V 088e43df.31a011e1.8195.00:01:84:2b:ec:ee 1 0 4efb90f3
^B 088e43db.31a011e1.8195.00:01:84:2b:ec:ee 1 0 0
^V 088e43d7.31a011e1.8195.00:01:84:2b:ec:ee 0 0 4efb90f3
^B 088e43d3.31a011e1.8195.00:01:84:2b:ec:ee 0 0 0
^E 088e43cf.31a011e1.8195.00:01:84:2b:ec:ee
^I 1 1 1

^I 2 1 1
4444
^I 3 1 1
5555
```
I.e. it works: only the diff, `5555` was added to the source container!
## Effect on annotate ##
Now, `annotate` gets confused...
It seems it cannot match the version name corresponding to the oid, with the one it would construct from the dependency graph:
```
cleartool: Error: Unknown version "I b17e438b.31a011e1.961f.00:15:60:04:45:5c 4444
```
although:
```
bar> ct des -s oid:b17e438b.31a011e1.961f.00:15:60:04:45:5c
/local/scratch/bar/a@@/main/mg-015/1
```
The confusion is however very limited, and arguably not introduced by our change. Annotate works on the assumption that (unless the -all flag is given), all contributors belong to the same _line of descent_. This assumption is defeated by any merge (not only ones in the context of our branching strategy). The glitch is fixed by injecting the -all flag, and pruning the spurious _UNRELATED_ tags produced.
## Generating cleartext containers ##
But `annotate` is not critical. Especially, it can be fixed within the wrapper, i.e. without requiring a new type manager, and its installation on every single ClearCase client.
The critical operation is the generation of cleartext containers, and **it** works:
```
bar> ct lsgen -d 3 -a a
a@@/main/mg/1 
 a@@/main/mg-015/1 (MG, MG_1.01)
  a@@/main/mg-014/1 (MG_1.00)
   a@@/main/0 
bar> ct dump a@@/main/mg/1 | grep cont=
source cont="/cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-f78e43a731a111e1961f00156004455c-6j"
clrtxt cont="/cc/views07/emagiro/bar.vbs/c/cdft/37/42/f78e43a731a111e1961f00156004455c"
bar> ct dump a@@/main/mg-015/1 | grep cont=
source cont="/cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-f78e43a731a111e1961f00156004455c-6j"
clrtxt cont="/cc/views07/emagiro/bar.vbs/c/cdft/2a/10/b17e438b31a011e1961f00156004455c"
bar> ct dump a@@/main/mg-014/1 | grep cont=
source cont="/cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-f78e43a731a111e1961f00156004455c-6j"
clrtxt cont="/cc/views07/emagiro/bar.vbs/c/cdft/a/25/fa3e436f319f11e1961f00156004455c"
bar> ll /cc/views07/emagiro/bar.vbs/c/cdft/37/42/f78e43a731a111e1961f00156004455c /cc/views07/emagiro/bar.vbs/c/cdft/2a/10/b17e438b31a011e1961f00156004455c /cc/views07/emagiro/bar.vbs/c/cdft/a/25/fa3e436f319f11e1961f00156004455c
-r--r--r--   1 emagiro  at1            6 Dec 28 22:10 /cc/views07/emagiro/bar.vbs/c/cdft/2a/10/b17e438b31a011e1961f00156004455c
-r--r--r--   1 emagiro  at1           11 Dec 28 22:20 /cc/views07/emagiro/bar.vbs/c/cdft/37/42/f78e43a731a111e1961f00156004455c
-r--r--r--   1 emagiro  at1            1 Dec 28 22:03 /cc/views07/emagiro/bar.vbs/c/cdft/a/25/fa3e436f319f11e1961f00156004455c
bar> rm -f /cc/views07/emagiro/bar.vbs/c/cdft/a/25/fa3e436f319f11e1961f00156004455c
bar> cat a@@/main/mg-014/1

bar> rm -f /cc/views07/emagiro/bar.vbs/c/cdft/37/42/f78e43a731a111e1961f00156004455c
bar> cat a@@/main/mg-015/1

4444
bar> rm -f /cc/views07/emagiro/bar.vbs/c/cdft/2a/10/b17e438b31a011e1961f00156004455c
bar> cat a@@/main/mg/1

4444
5555
```
## Other functions of the type manager ##
```
text_file_delta> pwd
/opt/rational/clearcase/lib/mgrs/text_file_delta
text_file_delta> ls -l
total 76
lrwxrwxrwx   1 root     other          6 Aug  3  2007 annotate -> tfdmgr
lrwxrwxrwx   1 root     other         22 Aug  3  2007 compare -> ../../../bin/cleardiff
lrwxrwxrwx   1 root     other          6 Aug  3  2007 construct_version -> tfdmgr
lrwxrwxrwx   1 root     other          6 Aug  3  2007 create_branch -> tfdmgr
lrwxrwxrwx   1 root     other          6 Aug  3  2007 create_element -> tfdmgr
lrwxrwxrwx   1 root     other          6 Aug  3  2007 create_version -> tfdmgr
lrwxrwxrwx   1 root     other          6 Aug  3  2007 delete_branches_versions -> tfdmgr
lrwxrwxrwx   1 root     other          6 Aug  3  2007 get_cont_info -> tfdmgr
lrwxrwxrwx   1 root     other         22 Aug  3  2007 merge -> ../../../bin/cleardiff
-r-xr-xr-x   1 root     sys        27368 Apr 30  2008 tfdmgr
lrwxrwxrwx   1 root     other         23 Aug  3  2007 xcompare -> ../../../bin/xcleardiff
lrwxrwxrwx   1 root     other         23 Aug  3  2007 xmerge -> ../../../bin/xcleardiff
```
Of these, _delete\_branches\_versions_ is the one to investigate:
### rmbranch ###
```
bar> ct rmbranch -f a@@/main/mg-015
text_file_delta: Error: Version 0x1 is still active on branch 0x2 in "/cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-f78e43a731a111e1961f00156004455c-6j"
cleartool: Error: Type manager "text_file_delta" failed delete_branches_versions operation.
cleartool: Error: Unable to remove branch "a@@/main/mg-015".
```
This is excellent: _rmbranch_ is _safer_ in this scheme than in the usual one! To actually remove the branch, one would need to remove first and explicitly the 'sub'-branch. It may be disputed whether this is annoying or safe: all too often though, administrators would skip this thinking and implement a trigger to produce the above result.
The important is that the operation does not result in loss of consistency.
### rmver ###
```
bar> ct rmver -f a@@/main/mg-015/1
cleartool: Error: Removal of "interesting" versions must be explicitly enabled.
Not removing these "interesting" versions of "a":
  /main/mg-015/1 (has: labels, hyperlinks)
cleartool: Error: No versions of "a" to remove.
bar> ct rmver -f -xhl -xla a@@/main/mg-015/1
Removing these versions of "a":
  /main/mg-015/1 (has: labels, hyperlinks)
Removed versions of "a".
bar> ct lsvtree -a -s -merge a
a@@/main
a@@/main/0
a@@/main/mg-014
a@@/main/mg-014/0
a@@/main/mg-014/1
a@@/main/mg-015
a@@/main/mg-015/0
a@@/main/mg
a@@/main/mg/0
a@@/main/mg/1
bar> ct ls a
a@@/main/mg/1                                            Rule: .../mg/LATEST
bar> cat a

4444
5555
bar> ct dump a@@/main/mg/1 | grep 'source cont=' | cut -d= -f2 | tr -d \"
/cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-b17e438b31a011e1961f00156004455c-58
bar> cat /cc/views07/emagiro/bar.vbs/s/sdft/13/32/0-b17e438b31a011e1961f00156004455c-58
^S db1 3
^V f78e43a7.31a111e1.961f.00:15:60:04:45:5c 3 1 4efb95e6
^V a70e5565.31a111e1.9d2e.00:01:84:2b:ec:ee 3 0 4efb9558
^B a70e5561.31a111e1.9d2e.00:01:84:2b:ec:ee 3 2 1
^#^V b17e438b.31a011e1.961f.00:15:60:04:45:5c 2 1 4efb93c2
^V bc8e4889.319f11e1.87ca.00:01:84:2b:ec:ee 2 0 4efb9220
^B bc8e4885.319f11e1.87ca.00:01:84:2b:ec:ee 2 1 1
^V fa3e436f.319f11e1.961f.00:15:60:04:45:5c 1 1 4efb90f3
^V 088e43df.31a011e1.8195.00:01:84:2b:ec:ee 1 0 4efb90f3
^B 088e43db.31a011e1.8195.00:01:84:2b:ec:ee 1 0 0
^V 088e43d7.31a011e1.8195.00:01:84:2b:ec:ee 0 0 4efb90f3
^B 088e43d3.31a011e1.8195.00:01:84:2b:ec:ee 0 0 0
^E 088e43cf.31a011e1.8195.00:01:84:2b:ec:ee
^I 1 1 1

^I 2 1 1
4444
^I 3 1 1
5555
```
This result was suprising, as differing in philosophy with the previous one. Note that this behaviour is **not** the same with cascading branches: then, the removal of _interesting versions_ must be explicitly enabled, in this case with the _-xbr_ flag, and the sub-branch gets **really** removed.
In any case, the important is once again that consistency of data is preserved, this time by _commenting away_ the critical version instead of really removing it. This possibility is documented (_tag delete_) in the excellent whitepaper on [type managers](http://www-01.ibm.com/support/docview.wss?uid=swg27009697&aid=1) (by Laurent Maréchal).
### merge ###
I decided, maybe a bit fast, that merging would only concern cleartext containers. I now successfully tested the actual effect.
### synchronization ###
Export and import work fine.
Of course, the new source container layout is not replicated...
# Other element types than text file #
The `xml` element type is in the `binary_delta_file` hierarchy.
For elements of this type, the source container is shared _per branch_, in the case of branches spawn from `/main/0`, or maybe otherwise independent...
```
aa> ct lsvtree -s -merge a.xml
a.xml@@/main
a.xml@@/main/0
a.xml@@/main/mg-006
a.xml@@/main/mg-006/2
a.xml@@/main/mg-007
a.xml@@/main/mg-007/1
aa> ct dump a.xml@@/main/mg-006/1 | grep cont=
source cont="\\EV68B599EABE1A\CC_STG\foo.vbs\s\sdft\19\4\yq-66bae1df67d54d2c98175d565699b4bc-ct"
clrtxt cont="\\EV68B599EABE1A\CC_STG\foo.vbs\c\cdft\27\0\d578cd122ef44dec8480f6e9f0f4f43a"
aa> ct dump a.xml@@/main/mg-006/2 | grep cont=
source cont="\\EV68B599EABE1A\CC_STG\foo.vbs\s\sdft\19\4\yq-66bae1df67d54d2c98175d565699b4bc-ct"
clrtxt cont="\\EV68B599EABE1A\CC_STG\foo.vbs\c\cdft\19\14\66bae1df67d54d2c98175d565699b4bc"
aa> ct dump a.xml@@/main/mg-007/1 | grep cont=
source cont="\\EV68B599EABE1A\CC_STG\foo.vbs\s\sdft\19\4\yq-fc8dac6b565641fa8974d7f579408c2c-3s"
clrtxt cont="\\EV68B599EABE1A\CC_STG\foo.vbs\c\cdft\10\2b\fc8dac6b565641fa8974d7f579408c2c"
```
Duplication of data takes thus place in a new file, instead of as for text files, in the same one.
The source container is compressed, and even after being uncompressed, contains binary data (some stripped from/converted to textual representation in the below transcript):
```
aa> file /cygdrive/c/CC_STG/foo.vbs/s/sdft/19/4/yq-66bae1df67d54d2c98175d565699b4bc-ct
/cygdrive/c/CC_STG/foo.vbs/s/sdft/19/4/yq-66bae1df67d54d2c98175d565699b4bc-ct: gzip compressed data, was "<fd:1>", from NTFS filesystem (NT)
aa> gunzip -cd /cygdrive/c/CC_STG/foo.vbs/s/sdft/19/4/yq-66bae1df67d54d2c98175d565699b4bc-ct > /tmp/foo
aa> cat /tmp/foo | tr '\000' '\n' | egrep -v '^$'
BR
2c7618ca.caec44b2.aaf0.fc:76:25:1a:21:57
4e9c601b
<xml>
1 2 3 4
</xml>


66bae1df.67d54d2c.9817.5d:56:56:99:b4:bc
4e9c61d0
2
3c8a4663.043848ac.a2ac.48:8c:27:eb:5d:f4
3c8a4663.043848ac.a2ac.48:8c:27:eb:5d:f4
d578cd12.2ef44dec.8480.f6:e9:f0:f4:f4:3a
1
d578cd12.2ef44dec.8480.f6:e9:f0:f4:f4:3a
4e9c601b
1
3c8a4663.043848ac.a2ac.48:8c:27:eb:5d:f4
3c8a4663.043848ac.a2ac.48:8c:27:eb:5d:f4
2eb25e4d.956541e4.82ee.c5:99:ef:04:b5:0c
0

	
^S
^U
^U
aa> ct des -s oid:2eb25e4d.956541e4.82ee.c5:99:ef:04:b5:0c oid:3c8a4663.043848ac.a2ac.48:8c:27:eb:5d:f4 oid:66bae1df.67d54d2c.9817.5d:56:56:99:b4:bc oid:d578cd12.2ef44dec.8480.f6:e9:f0:f4:f4:3a 
/foo/aa/a.xml@@/main/mg-006/0
/foo/aa/a.xml@@/main/mg-006
/foo/aa/a.xml@@/main/mg-006/2
/foo/aa/a.xml@@/main/mg-006/1
```
Checked what happens with a cascading branch:
```
aa> cto mkbranch -nc mg a.xml@@/main/mg-006/2
Created branch "mg" from "a.xml" version "\main\mg-006\2".
Checked out "a.xml" from version "\main\mg-006\mg\0".
aa> cat a.xml
<xml>
1 2 3 4
</xml>
aa> perl -pi -le 'print qq(<add>\n5 6 7 8\n</add>) if m%/xml%' a.xml
aa> cat a.xml
<xml>
1 2 3 4
<add>
5 6 7 8
</add>
</xml>
aa> ct ci -nc a.xml
Checked in "a.xml" version "/main/mg-006/mg/1".
aa> ct dump a.xml | grep 'source cont=' | cut -d= -f2 | tr -d \"
\\EV68B599EABE1A\CC_STG\foo.vbs\s\sdft\19\4\yq-a2b7a4006f6a401aa5673dadc093c170-7p
aa> gunzip -cd /cygdrive/c/CC_STG/foo.vbs/s/sdft/19/4/yq-a2b7a4006f6a401aa5673dadc093c170-7p >/tmp/out
aa> cat /tmp/out | tr '\000' '\n' | egrep -v '^$'
BR
2c7618ca.caec44b2.aaf0.fc:76:25:1a:21:57
4e9c601b
*<xml>
1 2 3 4
<add>
5 6 7 8
</add>
</xml>
a2b7a400.6f6a401a.a567.3d:ad:c0:93:c1:70
4f0052b1
1
8b314a87.188b493c.ac9f.d6:4d:2b:83:54:ca
8b314a87.188b493c.ac9f.d6:4d:2b:83:54:ca
0973ac0a.b9174924.b62e.4a:9e:7b:10:db:36
0

*
*
aa> ct dump a.xml@@/main/mg-006/mg/1 | grep 'source cont=' | cut -d= -f2 | tr -d \"
\\EV68B599EABE1A\CC_STG\foo.vbs\s\sdft\19\4\yq-a2b7a4006f6a401aa5673dadc093c170-7p
aa> ct dump a.xml@@/main/mg-006/2 | grep 'source cont=' | cut -d= -f2 | tr -d \"
\\EV68B599EABE1A\CC_STG\foo.vbs\s\sdft\19\4\yq-0973ac0ab9174924b62e4a9e7b10db36-qb
```
So, the source container is **not** shared either in the cascading branch case! It is not our non-cascading scheme which introduces the duplication of data!
# Implementation #
I did one tentative implementation (coming as part of 0.32--will be in 0.32c).
As far as I can tell, based on tests more than on real use yet, it works fine.

There are several issues.

The root cause of the problem is that editing the container must be done as _vob owner_, whereas the checkout/mkbranch command triggering it is a user operation.

This means that I may use a similar technology as the one used for locking/unlocking label types.

This works fine for vobs served on UNIX, but is more troublesome for ones served on Windows. There are various schemes offered: using _sudo_ or _ssh_, and involving a _suid_ enabled script and a common account.
Of those, only _ssh_ is potentially relevant (unless a _sudo_ Perl module is available with a port on Windows...) One needs to install an _ssh_ server on Windows (e.g. on cygwin), or to use a proxy.

Then, one'll meet the maintenance issue of authorizing the users to passwordlessly run a specific command as vob owner. This is what the _suid_ enabled script allows to bypass, hence requiring a UNIX proxy.

Finally (for Windows), vob storage is protected with ACL, which make it impossible to edit files _in-place_. One needs to make a copy, hence to take care of the end-of-line conventions and of preserving the ACL.

On UNIX, the only issue is one of performance: what is acceptable for _lock/unlock_ may be disputable for _checkout_...
One must consider spawing one process in the background, and using it for multiple files.

## One anticipated issue ##
The ability to remove a version from the _genealogy_ of another one may be part of a scheme to _rollback_ a change, while preserving later fixes or enhancements.

This event would be akin to a functionality termed in Git as _rebase_ (oh well... maybe not quite).

Obviously, this rollback functionality is simpler to implement if one doesn't have to alter the source container.