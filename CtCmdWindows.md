# Introduction #

Installing [ClearCase::CtCmd](http://search.cpan.org/search?query=ClearCase%3A%3ACtCmd&mode=all) on Windows may not be trivial.

# Details #

You need a compiler first, Visual Studio C more precisely.

It is available for free from Microsoft, for non-commercial purposes,
as the Express Edition.
Building your own perl modules is as I understand it, acceptable.

Go to: http://www.microsoft.com/express/
Get the version suitable for your Windows flavour:
2005 for Windows 2000, 2008 for newer.

With the 2008 version on Vista, I just start a command shell from the VC++ menu and it is enough.
With the 2005 version on Windows 2000, I had in addition to install the Windows (or Platform) SDK.
Search for it on the previous page.
Get again the one suitable for you.

I needed to set a few paths to make use of them.
But I had then to set two environment strings (on w2k):
```
set lib=C:\PROGRA~1\MIC977~1\Lib;C:\Progra~1\MICROS~4\VC\lib
set inc=-I"C:\PROGRA~1\MIC977~1\Include"
```
These paths are of course mine, with short names got from `dir /x`
(some 8bitpath setting enabled...)