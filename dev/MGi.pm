package ClearCase::Wrapper::MGi;

$VERSION = '0.11';

use warnings;
use strict;
use vars qw($ct $eqhl $prhl $diat);
($eqhl, $prhl, $diat) = qw(EqInc PrevInc DelInc);

sub compareincs($$) {
  my ($t1, $t2) = @_;
  my ($p1, $M1, $m1, $s1) = pfxmajminsfx($t1);
  my ($p2, $M2, $m2, $s2) = pfxmajminsfx($t2);
  if (!(defined($p1) and defined($p2) and ($p1 eq $p2) and ($s1 eq $s2))) {
    warn Msg('W', "$t1 and $t2 not comparable\n");
    return 1;
  }
  return (($M1 <=> $M2) or (defined($m1) and defined($m2) and $m1 <=> $m2));
}
use AutoLoader 'AUTOLOAD';
use ClearCase::Wrapper;

#############################################################################
# Usage Message Extensions
#############################################################################
{
  local $^W = 0;
  no strict 'vars';

  my $z = $ARGV[0] || '';
  $lsgenealogy =
    "$z [-short] [-all] [-obsolete] [-depth gen-depth] pname ...";
  $mklbtype = "\n* [-family] [-increment] [-archive] pname ...";
  $mklabel	= "\n* [-up] [-force]";
}

#############################################################################
# Command Aliases
#############################################################################
*co             = *checkout;
*lsgen		= *lsgenealogy;
*unco           = *uncheckout;

1;

__END__

## Internal service routines, undocumented.
sub sosbranch($$) {		# same or sub- branch
  my ($cur, $prd) = @_;
  $cur =~ s:/([0-9]+|CHECKEDOUT)$:/:;
  $prd =~ s:/([0-9]+|CHECKEDOUT)$:/:;
  return ($cur =~ qr(^$prd));
}
sub printparents {
  my ($id, $gen, $seen, $ind) = @_;
  if ($$seen{$id}++) {
    printf("%${ind}s\[alternative path: ${id}\]\n", '')
      unless $opt{short};
    return;
  }
  my @p = defined($$gen{$id}{parents}) ? @{ $$gen{$id}{parents} } : ();
  my @c = defined($$gen{$id}{children}) ? @{ $$gen{$id}{children} } : ();
  my $l = defined($$gen{$id}{labels}) ? " $$gen{$id}{labels}" : '';
  my (@s, @u) = ();
  foreach my $c (@c) {
    if ($$seen{$c}) {
      push @s, $c;
    } else {
      push @u, $c;
    }
  }
  if (scalar(@u) and !$opt{short}) {
    my $cprinted = 0;
    for my $c (@s) {
      $cprinted++ if $$gen{$c}{printed};
    }
    if ($cprinted) {
      if ($ind == 0) {
	print "\[offsprings:";
      } else {
	my $pind = $ind - 1;
	printf("%${pind}s\[siblings:", '');
      }
      foreach (@u) {
	print " $_";
      }
      print "\]\n";
    }
  }
  if ($opt{all}
	or $l
	  or (scalar(@p) != 1)
	    or (scalar(@s) != 1)
	      or !sosbranch($id, $s[0])
		or !sosbranch($p[0], $id)) {
    if ($opt{short}) {
      if ((scalar(@p) != 1)
	    or (scalar(@s) != 1)
	      or !sosbranch($id, $s[0])
		or !sosbranch($p[0], $id)) {
	printf("%${ind}s${id}\n", '');
	$$gen{$id}{printed}++;
	${ind}++;
      }
    } else {
      printf("%${ind}s${id}${l}\n", '');
      $$gen{$id}{printed}++;
      ${ind}++;
    }
  }
  return if (defined($opt{depth})) and ($opt{depth} < $ind);
  foreach my $p (@p) {
    if ($$gen{$id}{depth} < $$gen{$p}{depth}) {
      printparents($p, $gen, $seen, $ind);
    } else {
      printf("%${ind}s\[alternative path: ${p}\]\n", '')
	unless $opt{short};
    }
  }
}
sub findpredinstack($$) {
  my ($g, $stack) = @_;
  while ($$stack[-1]) {
    return $$stack[-1] if sosbranch($g, $$stack[-1]);
    pop @{$stack};
  }
  return 0; 
}
sub setdepths {
  no warnings 'recursion';
  my ($id, $dep, $gen) = @_;
  if (defined($$gen{$id}{depth})) {
    if ($$gen{$id}{depth} > $dep) {
      $$gen{$id}{depth} = $dep;
    } else {
      return;
    }
  } else {
    $$gen{$id}{depth} = $dep;
  }
  my @p = defined($$gen{$id}{parents}) ? @{ $$gen{$id}{parents} } : ();
  foreach my $p (@p) {
    setdepths($p, $$gen{$id}{depth} + 1, $gen);
  }
}
sub checkcs {
  use File::Basename;
  use Cwd;
  my ($v) = @_;
  $ct = $ct->clone();
  $v =~ s/^(.*?)\@\@.*$/$1/;
  my $dest = dirname($v);
  my $pwd = getcwd();
  chdir($dest);
  my @cs = grep /^\#\#:BranchOff: *root/, $ct->argv('catcs')->qx;
  chdir($pwd) if $pwd;
  return scalar @cs;
}
sub pbrtype {
  my ($pbrt, $bt) = @_;
  if (!defined($pbrt->{$bt})) {
    my $tc = $ct->argv('des', qw(-fmt %[type_constraint]p),
		       "brtype:$bt")->qx;
    $pbrt->{$bt} = ($tc =~ /one version per branch/);
  }
  return $pbrt->{$bt};
}
sub parsevtree($$$) {
  my ($ele, $obs, $sel) = @_;
  $ct->argv('lsvtree');
  my @opt = qw(-merge -all);
  push @opt, '-obs' if $obs;
  $ct->opts(@opt);
  my @vt = grep m%(^$sel|[\\/]([1-9]\d*|CHECKEDOUT))( .*)?$%,
    $ct->args($ele)->qx;
  map { s%\\%/%g } @vt;
  my %gen = ();
  my @stack = ();
  foreach my $g (@vt) {
    $g =~ s%^(.*/CHECKEDOUT) view ".*"(.*)$%$1$2%;
    if ($g =~ m%^(.*) (\(.*\))$%) {
      $g = $1;
      $gen{$g}{labels} = $2;
    }
    if ($g =~ /^  -> (.*)$/) {
      my $v = $1;
      my $n = "$ele\@\@$v";
      push @{ $gen{$stack[-1]}{children} }, $n;
      push @{ $gen{$n}{parents} }, $stack[-1];
      next;
    }
    if (findpredinstack($g, \@stack)) {
      push @{ $gen{$g}{parents} }, $stack[-1];
      push @{ $gen{$stack[-1]}{children} }, $g;
    }
    push @stack, $g;
  }
  return %gen;
}
sub mkbco($$$$$$) {
  my ($e, $bt, $pbrt, $bopt, $gopt, $copt) = @_;
  my $typ = $ct->argv(qw(des -fmt %m), $e)->qx;
  if ($typ !~ /(branch|version)$/) {
    warn Msg('W', "Not a vob object: $e");
    return 1;
  }
  my $ver;
  if ($e =~ m%^(.*?)\@\@.*$%) {
    $ver = $e;
    $e = $1;
    $ver =~ s%[\\/]$%%;
    $ver .= '/LATEST' if $typ eq 'branch';
  }
  if (!$ver or !$bt) {
    my $sel = $ct->argv('ls', '-d', $e)->qx;
    if ($bt) {
      $ver = $1 if $sel =~ /^(.*?) +Rule/;
    } elsif ($sel =~ /^(.*?) +Rule:.*-mkbranch (.*?)\]?$/) {
      ($ver, $bt) = ($ver? $ver : $1, $2);
    }
  }
  if ($bt and checkcs($ct, $e)) {
    my $main = ($ct->argv('lsvtree', $e)->qx)[0];
    $main =~ s%^[^@]*\@\@[\\/](.*)$%$1%;
    my $vob = $ct->argv('des', '-s', "vob:$e")->qx;
    my $re = pbrtype($pbrt, "$bt\@$vob") ?
      qr([\\/]${main}[\\/]$bt[\\/]\d+$) : qr([\\/]$bt[\\/]\d+$);
    if ($ver =~ m%$re%) {
      push @$gopt, @$copt, $e;
      return $ct->argv('co', @$gopt)->system;
    } else {
      my @mkbcopt = @$copt? @$copt : qw(-nc);
      my $r = $ct->argv('mkbranch', @mkbcopt,
			'-ver', "/${main}/0", $bt, $e)->system;
      if ($r) {
	return 1;
      } else {
	if ($ver !~ m%\@\@[\\/]${main}[\\/]0$%) {
	  my $rc = $ct->argv('merge', '-to', $e, $ver)->stdout(0)->system;
	  unlink glob("$e.contrib*");
	  return $rc;
	}
      }
    }
  } else {
    push @$gopt, @$bopt, @$copt, $e; # Ensure non empty array
    return $ct->argv('co', @$gopt)->system;
  }
}
sub ensuretypes(@) {
  my @vob = shift;
  my %cmt = ($eqhl => q(Equivalent increment),
	     $prhl => q(Previous increment in a type chain),
	     $diat => q(Deleted in increment));
  my $silent = $ct->clone;
  $silent->stdout(0);
  $silent->stderr(0);
  for my $t ($eqhl, $prhl) {
    for my $v (@vob) {
      my $t2 = "$t\@$v";
      if ($silent->argv(qw(des -s), "hltype:$t2")->system) {
	$ct->argv(qw(mkhltype -c), $cmt{$t}, $t2)->system and die;
      }
    }
  }
  for my $v (@vob) {
    if ($silent->argv(qw(des -s), "attype:$diat\@$v")->system) {
      $ct->argv(qw(mkattype -c), $cmt{$diat}, "$diat\@$v")->system and die;
    }
  }
}
sub pfxmajminsfx($) {
  my $t = shift;
  if ($t =~ /^(\w+[-_])(\d+)(?:\.(\d+))?(\@.*)?$/) {
    my $min = ($3 or '');
    my $sfx = ($4 or '');
    return ($1, $2, $min, $sfx);
  } else {
    warn Msg(
      'W', "$t doesn't match the pattern expected for an incremental type\n");
  }
}
sub nextinc($) {
  my $inc = shift;
  my ($pfx, $maj, $min, $sfx) = pfxmajminsfx($inc);
  return '' unless $pfx and $maj;
  my $count = defined($min)? $maj . q(.) . ++$min : ++$maj;
  return $pfx . $count . $sfx;
}
sub findnext($) {		# on input, the type exists
  my $c = shift;
  my @int = grep { s/^<- lbtype:(.*)$/$1/ }
    $ct->argv(qw(des -s -ahl), $prhl, "lbtype:$c")->qx;
  if (@int) {
    my @i = ();
    for (@int) {
      push @i, findnext($_);
    }
    return @i;
  } else {
    return ($c);
  }
}
sub findfreeinc($) {	   # on input, the values may or may not exist
  my $nxt = shift;
  while (my ($k, $v) = each %{$nxt}) {
    if ($ct->argv(qw(des -s), "lbtype:$v")->stderr(0)->qx) { #exists
      my @cand = sort compareincs findnext($v);
      $$nxt{$k} = nextinc($cand[$#cand]);
    }
  }
}

=head1 NAME

ClearCase::Wrapper::MGi - Marc Girod's contributed cleartool wrapper functions

=head1 SYNOPSIS

This is an C<overlay module> for B<ClearCase::Wrapper> containing Marc
Girod's non-standard extensions. See C<perldoc ClearCase::Wrapper> (by
David Boyce) for more details.

=head1 CLEARTOOL EXTENSIONS

=over 9

=item * LSGENEALOGY

New command. B<LsGenealogy> is an alternative way to display the
version tree of an element. It will treat merge arrows on a par level
with parenthood on a branch, and will navigate backwards from the
version currently selected, to find what contributors took part in its
state.
This is thought as being particularly adapted to displaying the
bush-like structure characteristic of version trees produced under the
advocated branching strategy.

Flags:

=over 4

=item B<-all>

Show 'uninteresting' versions, otherwise skipped:

=over 2

=item - bearing no label

=item - not at a chain boundary.

=back

=item B<-obsolete>

Add obsoleted branches to the search.

=item B<-short>

Skip displaying labels and 'labelled' versions and do not report
alternative paths or siblings.

=item B<-depth>

Specify a maximum depth at which to stop displaying the genealogy of
the element.

=back

=cut

sub lsgenealogy {
  GetOptions(\%opt, qw(short all obsolete depth=i));
  Assert(@ARGV > 1);		# die with usage msg if untrue
  shift @ARGV;
  my @argv = ();
  for (@ARGV) {
    $_ = readlink if -l && defined readlink;
    push @argv, MSWIN ? glob($_) : $_;
  }
  $ct = ClearCase::Argv->new({autofail=>0,autochomp=>1,stderr=>0});
  $ct->ipc(1) unless $ct->ctcmd(1);
  while (my $e = shift @argv) {
    my ($ele, $ver, $type) =
      $ct->argv(qw(des -fmt), '%En\n%En@@%Vn\n%m', $e)->qx;
    if (!defined($type) or ($type !~ /version$/)) {
      warn Msg('W', "Not a version: $e");
      next;
    }
    $ele =~ s%\\%/%g;
    $ver =~ s%\\%/%g;
    my %gen = parsevtree($ele, $opt{obsolete}, $ver);
    setdepths($ver, 0, \%gen);
    my %seen = ();
    printparents($ver, \%gen, \%seen, 0);
  }
  exit 0;
}

=item * CO/CHECKOUT

Supports the BranchOff feature, which you can set up via an attribute
in the config spec.  The rationale and the design are documented in:

 http://www.cmwiki.com/BranchOffMain0

Instead of branching off the selected version, the strategy is to
branch off the root of the version tree, copy-merging there from the
former.

This allows to avoid both merging back to /main or to a delivery
branch, and to cascade branches indefinitely.  The logical version tree
is restituted by navigating the merge arrows, to find all the direct or
indirect contributors.

Flag:

=over 1

=item B<-ver/sion>

Ignored under a I<BranchOff> config spec,
but the version specified in the pname is anyway obeyed,
as a branch may always be spawn.

=back

=cut

sub checkout {
  for (@ARGV[1..$#ARGV]) {
    $_ = readlink if -l && defined readlink;
  }
  $ct = ClearCase::Argv->new({autochomp=>1});
  $ct->ipc(1) unless $ct->ctcmd(1);
  # Duplicate the base Wrapper checkout functionality.
  my @agg = grep /^-(?:dir|rec|all|avo)/, @ARGV;
  die Msg('E', "mutually exclusive flags: @agg") if @agg > 1;
  if (@agg) {
    # Remove the aggregation flag, push the aggregated list of
    # not-checked-out file elements onto argv, and return.
    my %opt;
    GetOptions(\%opt, qw(directory recurse all avobs ok));
    my @added = AutoNotCheckedOut($agg[0], $opt{ok}, 'f', @ARGV);
    push(@ARGV, @added);
  }
  return 0 if grep /^-bra/, @ARGV[1..$#ARGV];
  my %opt;
  GetOptions(\%opt, qw(reserved unreserved nmaster out=s ndata ptime
		       nwarn c=s cfile=s cq cqe nc version branch=s
		       query nquery usehijack));
  if (my @o = grep /^-/, @ARGV) {
    if (!(grep /^--$/, @o)) {
      warn Msg('E', "Unsupported options: @o");
      exit(1);
    }
  }
  my @bopts = ();
  push @bopts, q(-ver) if $opt{version};
  my @gopts = ();
  push @gopts, q(-res) if $opt{reserved};
  push @gopts, q(-unr) if $opt{unreserved};
  push @gopts, q(-nma) if $opt{nmaster};
  push @gopts, q(-out), $opt{out}     if $opt{out};
  push @gopts, q(-nda) if $opt{ndata};
  push @gopts, q(-pti) if $opt{ptime};
  push @gopts, q(-nwa) if $opt{nwarn};
  push @gopts, q(-cfi), $opt{cfile}   if $opt{cfile};
  push @gopts, q(-cq)  if $opt{cq};
  push @gopts, q(-cqe) if $opt{cqe};
  push @gopts, q(-que) if $opt{query};
  push @gopts, q(-nqu) if $opt{nquery};
  push @gopts, q(-use) if $opt{usehijack};
  my @copt = ();
  push @copt, q(-c), $opt{c} if $opt{c};
  push @copt, q(-nc)         if $opt{nc};
  my @args = ();
  if (MSWIN) {
    for (@ARGV[1..$#ARGV]) {
      push @args, glob($_);
    }
  } else {
    @args = @ARGV[1..$#ARGV];
  }
  my $rc = 0;
  my %pbrt = ();
  foreach my $e (@args) {
    $rc |= mkbco($e, undef, \%pbrt, \@bopts, \@gopts, \@copt);
  }
  exit $rc;
}

=item * MKBRANCH

Actually a special case of checkout.

Flag:

=over 1

=item B<-nco>

Special case of reverting to the default behaviour,
as this cannot reasonably be served in a new branch under BranchOff
(no version to which to attach the I<Merge> hyperlink).

=back

=cut

sub mkbranch {
  for (@ARGV[1..$#ARGV]) {
    $_ = readlink if -l && defined readlink;
  }
  $ct = ClearCase::Argv->new({autochomp=>1});
  $ct->ipc(1) unless $ct->ctcmd(1);
  my @agg = grep /^-(?:dir|rec|all|avo)/, @ARGV;
  die Msg('E', "mutually exclusive flags: @agg") if @agg > 1;
  if (@agg) {
    # Remove the aggregation flag, push the aggregated list of
    # not-checked-out file elements onto argv, and return.
    my %opt;
    GetOptions(\%opt, qw(directory recurse all avobs ok));
    my @added = AutoNotCheckedOut($agg[0], $opt{ok}, 'f', @ARGV);
    push(@ARGV, @added);
  }
  return 0 if grep /^-nco/, @ARGV[1..$#ARGV];
  my %opt;
  GetOptions(\%opt, qw(c=s cfile=s cq cqe nc nwarn version));
  if (my @o = grep /^-/, @ARGV) {
    if (!(grep /^--$/, @o)) {
      warn Msg('E', "Unsupported options: @o");
      exit(1);
    }
  }
  my @bopts = ();
  push @bopts, q(-ver) if $opt{version};
  my @gopts = ();
  push @gopts, q(cfile), $opt{cfile} if $opt{cfile};
  push @gopts, q(cq)    if $opt{cq};
  push @gopts, q(cqe)   if $opt{cqe};
  push @gopts, q(nwarm) if $opt{nwarm};
  my @copt = ();
  push @copt, q(-c), $opt{c} if $opt{c};
  push @copt, q(-nc)         if $opt{nc};
  my @args = ();
  if (MSWIN) {
    for (@ARGV[1..$#ARGV]) {
      push @args, glob($_);
    }
  } else {
    @args = @ARGV[1..$#ARGV];
  }
  my $rc = 0;
  my %pbrt = ();
  my $bt = shift @args;
  $bt =~ s/^brtype:(.*)$/$1/;
  die if $ct->argv(qw(des -s), "brtype:$bt")->stdout(0)->system;
  foreach my $e (@args) {
    $rc |= mkbco($e, $bt, \%pbrt, \@bopts, \@gotps, \@copt);
  }
  exit $rc;
}

=item * DIFF

Evaluate the predecessor from the genealogy, i.e. take into account merges on
an equal basis as parents on the same physical branch.
In case there are multiple parents, consider the one on the same branch as
'more equal than the others' (least surprise principle).

Preserve the (Wrapper default) assumption of a B<-pred> flag, is only one
argument is given.

=cut

sub diff {
  for (@ARGV[1..$#ARGV]) {
    $_ = readlink if -l && defined readlink;
  }
  push(@ARGV, qw(-dir)) if @ARGV == 1;
  my $diff = ClearCase::Argv->new(@ARGV);
  $diff->autochomp(1);
  $diff->ipc(1) unless $diff->ctcmd(1);
  $diff->parse(qw(options=s serial_format|diff_format|window
		  graphical|tiny|hstack|vstack|predecessor));
  my @args = $diff->args;
  my @opts = $diff->opts;
  my $pred = grep /^-(pred)/, @opts;
  my $auto = grep /^-(?:dir|rec|all|avo)/, @args;
  my @elems = AutoCheckedOut(0, @args);
  return 0 unless $pred or $auto or (scalar @elems == 1);
  $diff->opts(grep !/-pred/, @opts) if $pred;
  $diff->args(@elems);
  $ct = $diff->clone();
  for my $e (@elems) {
    my ($ele, $ver, $type) =
      $ct->argv(qw(des -fmt), '%En\n%En@@%Vn\n%m', $e)->qx;
    if (!defined($type) or ($type !~ /version$/)) {
      warn Msg('W', "Not a version: $e");
      next;
    }
    $ele =~ s%\\%/%g;
    $ver =~ s%\\%/%g;
    my $bra = $1 if $ver =~ m%^(.*?)/(?:\d+|CHECKEDOUT)$%;
    my %gen = parsevtree($ele, 1, $ver);
    my $p = $gen{$ver}{'parents'};
    my ($brp) = grep { m%^$bra/\d+$% } @{$p};
    $ver = $ele if $ver =~ m%/CHECKEDOUT$%;
    $diff->args($brp? $brp : $p->[0], $ver)->system;
  }
  exit $?;
}

=item * UNCHECKOUT

The wrapper implements the functionality commonly provided by a trigger,
to remove the parent branch if it has no checkouts, no sub-branches, and
no remaining versions, while unchecking out version number 0.

=cut

sub uncheckout {
  my %opt;
  GetOptions(\%opt, qw(ok)) if grep /^-(dif|ok)/, @ARGV;
  for (@ARGV[1..$#ARGV]) {
    $_ = readlink if -l && defined readlink;
  }
  ClearCase::Argv->ipc(1) unless ClearCase::Argv->ctcmd(1);
  my $unco = ClearCase::Argv->new(@ARGV);
  $unco->parse(qw(keep rm cact cwork));
  $unco->optset('IGNORE');
  $unco->parseIGNORE(qw(c|cfile=s cqe|nc));
  $unco->args(sort {$b cmp $a} AutoCheckedOut($opt{ok}, $unco->args));
  $ct = ClearCase::Argv->new({autochomp=>1});
  my @b0 = grep { m%[\\/]CHECKEDOUT$% }
    $ct->argv(qw(ls -s -d), $unco->args)->qx;
  my $rc = $unco->system;
  map { s%^(.*)[\\/]CHECKEDOUT$%$1% } @b0;
  my @rm = ();
  for my $d (@b0) {
    opendir BR, $d or next;
    my @f = grep !/\.\.?/, readdir BR;
    closedir BR;
    push @rm, $d if (scalar @f == 2) and $f[0] eq '0' and $f[1] eq 'LATEST';
  }
  $ct->argv(qw(rmbranch -f), @rm)->system if @rm;
  exit $rc;
}

=item * MKLBTYPE

Extension: families of types, with a chain of fixed types, linked in a
succession, and one floating type, convenient for use in config specs.
One application is incremental types, applied only to modified versions,
allowing however to simulate full baselines.
This is implemented as part of UCM, for fixed types, and I<magic> config
specs. The wrapper offers thus a similar functionality on base ClearCase.

The current baseline is embodied with floating labels, which are moved
over successive versions. This floating type is however not mandatory.
It is created with the B<-fam/ily> flag. Families with no such floating
type are just chains, and can still be used with the B<-inc/rement> flag.

Types forming a family are related with hyperlinks of two types:

=over 2

=item B<EqInc>

=item B<PrevInc>

=back

One attribute:

=over 1

=item B<DelInc>

=back

Flags:

=over 4

=item B<-fam/ily>

Create two label types, linked with an B<EqInc> hyperlink.
The first, given as argument, will be considered as an alias for successive
increments of the second. It is the I<family> type.
The name of the initial incremental type is this of the I<family> type, with
a suffix of I<_1.00>.

=item B<-inc/rement>

Create a new increment of an existing label type family, given as argument.
This new type will take the place of the previous increment, as the
destination of the B<EqInc> hyperlink on the I<family> type.
It will have a B<PrevInc> hyperlink pointing to the previous increment in
the family.

=item B<arc/hive>

Rename the current type to an I<archive> value (name as prefix, and a
numeral suffix. Initial value: I<-001>), create a new type, and make the
archived one its predecessor, with a B<PrevInc> hyperlink.

=item B<-glo/bal>

Global types (in an Admin vob or not) are incompatible with the family
property.

=back

=cut

sub mklbtype {
  my (%opt, $rep);
  GetOptions(\%opt, qw(family increment archive));
  GetOptions('replace' => \$rep);
  return if !%opt and grep /^-(ord|glo)/, @ARGV;
  die Msg('E', 'Incompatible options: family increment archive')
    if keys %opt > 1;
  die Msg('E', 'Incompatible options: incremental types cannot be global')
    if %opt and grep /^-glo/, @ARGV;
  ClearCase::Argv->ipc(1);
  $ct = ClearCase::Argv->new({autochomp=>1});
  my $ntype = ClearCase::Argv->new(@ARGV);
  $ntype->parse(qw(global|ordinary vpelement|vpbranch|vpversion
		pbranch|shared gt|ge|lt|le|enum|default|vtype=s cqe|nc
		c|cfile=s));
  my @args = $ntype->args;
  my $silent = $ct->clone;
  $silent->stdout(0);
  if (!%opt and my ($ahl) = grep /^->/,
      $ct->desc([qw(-s -ahl AdminVOB vob:.)])->qx) {
    if (my $avob = (split /\s+/, $ahl)[1]) {
      for (@args) {
	next if /\@/;
	$_ = "$_\@$avob";
	warn Msg('W', "making global type $_ ...");
      }
      $ntype->args(@args);
      my @opts = ('-global', $ntype->opts);
      push @opts, '-replace' if $rep;
      $ntype->opts(@opts);
      $ntype->exec;
    }
  } elsif (%opt) {
    map { s/^lbtype:(.*)$/$1/ } @args;
    if ($rep) {
      @args = grep { $_ = $ct->argv(qw(des -s), "lbtype:$_")->qx } @args;
      exit 1 unless @args;
      if ($opt{family}) {
	my @a = ();
	foreach my $t (@args) {
	  if ($ct->argv(qw(des -s -ahl), $eqhl, "lbtype:$t")->stderr(0)->qx) {
	    warn Msg('E', "$t is already a family type\n");
	  } else {
	    push @a, $t;
	  }
	}
	exit 1 unless @a;
	my %pair = ();
	foreach (@a) {
	  if (/^(.*)(@@.*)?$/) {
	    $pair{$_} = "${1}_1.00" . ($2? $2:'');
	  }
	}
	findfreeinc(\%pair);
	$ntype->args(values %pair);
	$ntype->system;
	map {
	  if (defined($pair{$_})) {
	    my $inc = "lbtype:$pair{$_}";
	    $silent->argv('mkhlink', $eqhl, "lbtype:$_", $inc)->system;
	  }
	} keys %pair;
      } else {			# increment
	die Msg('E', "Incompatible flags: replace and incremental");
      }
    } else {
      my @a = @args;
      my @vobs = grep { s/.*\@(.*)$/$1/ } @a;
      push @vob, $ct->argv(qw(des -s vob:.))->stderr(0)->qx
	if grep !/@/, @args;
      die  Msg('E', qq(Unable to determine VOB for pathname ".".\n))
	unless @vob;
      ensuretypes(@vob);
      @a = @args;
      if ($opt{family}) {
	my %pair = ();
	foreach (@a) {
	  if (/^(.*)(@@.*)?$/) {
	    $pair{$_} = "${1}_1.00" . ($2? $2:'');
	  }
	}
	findfreeinc(\%pair);
	$ntype->args(@args, values %pair);
	$ntype->system;
	map {
	  if (defined($pair{$_})) {
	    my $inc = "lbtype:$pair{$_}";
	    $silent->argv('mkhlink', $eqhl, "lbtype:$_", $inc)->system;
	  }
	} keys %pair;
      } elsif($opt{increment}) {			# increment
	for my $t (@args) {
	  die
	    Msg('E',
		"Lock on label type \"$t\" prevents operation \"make lbtype\"")
	    if $ct->argv(qw(lslock -s),"lbtype:$t")->stderr(0)->qx;
	  my ($pair) = grep s/^\s*(.*) -> lbtype:(.*)\@(.*)$/$1,$2,$3/,
	    $ct->argv(qw(des -l -ahl), $eqhl, "lbtype:$t")->stderr(0)->qx;
	  my ($hlk, $prev, $vob) = split ',', $pair if $pair;
	  next unless $prev;
	  if ($prev =~ /^(.*)_(\d+)(?:\.(\d+))?$/) {
	    my ($base, $maj, $min) = ($1, $2, $3);
	    my $new = "${base}_" .
	      (defined($min)? $maj . '.' . ++$min : ++$maj);
	    $new .= $1 if $t =~ /^.*(@.*)$/;
	    $ntype->args($new)->system;
	    $silent->argv('rmhlink', $hlk)->system;
	    $silent->argv(qw(mkhlink -nc), $eqhl,
			  "lbtype:$t", "lbtype:$new")->system;
	    $silent->argv(qw(mkhlink -nc), $prhl,
			  "lbtype:$new", "lbtype:$prev")->system;
	  } else {
	    warn "Previous increment non suitable in $t: $prev\n";
	    next;
	  }
	}
      } else {
	die Msg('E', 'Not implemented yet');
      }
    }
    exit 0;
  } else {			# non inc
    if ($rep) {
      $ntype->opts('-replace', $ntype->opts);
      map { $_ = "lbtype:$_" unless /^lbtype:/ } @args;
      my @a = $ct->argv(qw(des -s), @args)->stderr(0)->qx;
      if (@a) {
	map { $_ = "lbtype:$_" } @a;
	my @link = grep s/^\s*(.*) -> .*$/$1/,
	  $ct->argv(qw(des -l -ahl), "$eqhl,$prhl", @a)->qx;
	$ct->argv('rmhlink', @link)->system;
      }
    }
  }
}

=item * LOCK

New B<-allow> and B<-deny> flags. These work like I<-nuser> but operate
incrementally on an existing I<-nuser> list rather than completely
replacing it. When B<-allow> or B<-deny> are used, I<-replace> is
implied.

When B<-iflocked> is used, no lock will be created where one didn't
previously exist; the I<-nusers> list will only be modified for
existing locks.

In case of a family type, lock also the equivalent incremental type.

There may be an issue if the two types are not owned by the same account.
You may overcome it by providing a module specification via the environment
variable B<FORCELOCK>. This module must export both a B<flocklt> and a
B<funlocklt> (force lock and unlock label type) functions.
The functions take an B<lbtype> and a B<vob tag> as input (B<flocklt>
optionally takes a B<replace> flag and an B<nusers> exception list).
The two functions take the responsibility of printing the standard output
(but not necessarily the errors), and return an error code: 0 for success,
other for error.
See the documentation for examples of implementation.

=cut

sub lock {
  my (%opt, $nusers);
  GetOptions(\%opt, qw(allow=s deny=s iflocked));
  GetOptions('nusers=s' => \$nusers);
  ClearCase::Argv->ipc(1);
  my $lock = ClearCase::Argv->new(@ARGV);
  $lock->parse(qw(c|cfile=s cquery|cqeach pname=s obsolete replace));
  die Msg('E', "cannot specify -nusers along with -allow or -deny")
    if %opt and $nusers;
  die Msg('E', "cannot use -allow or -deny with multiple objects")
    if %opt and $lock->args > 1;
  my $lslock = ClearCase::Argv->lslock([qw(-fmt %c)], $lock->args);
  my($currlock) = $lslock->autofail(1)->qx;
  if ($currlock && $currlock =~ m%^Locked except for users:\s+(.*)%) {
    my %nusers = map {$_ => 1} split /\s+/, $1;
    if ($nusers) {
      %nusers = ();
      map { $nusers{$_} = 1 } split /,/, $nusers;
    } else {
      if ($opt{allow}) {
	map { $nusers{$_} = 1 } split /,/, $opt{allow};
      } elsif ($opt{deny}) {
	map { delete $nusers{$_} } split /,/, $opt{deny};
      } else {
	%nusers = ();
      }
    }
    $lock->opts($lock->opts, '-nusers', join(',', sort keys %nusers))
      if %nusers;
  } elsif (($nusers or $opt{allow}) and
	     (!$currlock or $opt{iflocked}) or $lock->flag('replace')) {
    $lock->opts($lock->opts, '-nusers', ($nusers or $opt{allow}));
  }
  $lock->opts($lock->opts, '-replace')
    if ($opt{allow} or $opt{deny}) and $currlock and !$lock->flag('replace');
  my @args = $lock->args;
  $ct = ClearCase::Argv->new({autochomp=>1});
  my (@lbt, @oth, %vob);
  my $locvob = $ct->argv(qw(des -s vob:.))->stderr(0)->qx;
  foreach my $t (@args) {
    if ($ct->argv(qw(des -fmt), '%m\n', $t)->stderr(0)->qx eq 'label type') {
      my @et = grep s/^-> lbtype:(.*)@.*$/$1/,
	$ct->argv(qw(des -s -ahl), $eqhl, $t)->qx;
      if (@et) {
	my ($e, $p) = ($et[0], '');
	if ($t =~ /lbtype:(.*)@(.*)$/) {
	  $t = $1; $vob{$t} = $2;
	} else {
	  $t =~ s/^lbtype://;
	  $vob{$t} = $locvob;
	}
	my $v = $vob{$t}; $vob{$e} = $v;
	push @lbt, $t, $e;
	my @pt = grep s/^-> lbtype:(.*)@.*$/$1/,
	  $ct->argv(qw(des -s -ahl), $prhl, "lbtype:$e\@$v")->qx;
	if (@pt) {
	  $p = $pt[0];
	  if (!$ct->argv(qw(lslock -s), "lbtype:$p\@$v")->stderr(0)->qx) {
	    push @lbt, $p;
	    $vob{$p} = $v;
	  }
	}
      } else {
	push @oth, $t;
      }
    } else {
      push @oth, $t;
    }
  }
  my $rc = @oth? $lock->args(@oth)->system : 0;
  my ($fl, $loaded) = $ENV{FORCELOCK};
  for my $lt (@lbt) {
    my $v = $vob{$lt};
    if ($lock->args("lbtype:$lt\@$v")->stderr(0)->system) {
      if ($fl and !$loaded) {
	my $fn = $fl; $fn =~ s%::%/%g; $fn .= '.pm';
	require $fn;
	$fl->import;
	$loaded = 1;
      }
      if (!$fl or flocklt($lt, $v, $lock->flag('replace'),
			  ($nusers or $opt{allow}))) {
	warn Msg('E', "Could not lock lbtype:$lt\@$v");
	$rc = 1;
      }
    }
  }
  exit $rc;
}

=item * UNLOCK

In case of a family type, unlock also the equivalent incremental type.

There may be an issue if the two types are not owned by the same account.
See the B<LOCK> documentation for overcoming it with a B<FORCELOCK>
environment variable.

=cut

sub unlock() {
  ClearCase::Argv->ipc(1);
  my $unlock = ClearCase::Argv->new(@ARGV);
  $unlock->parse(qw(c|cfile=s cquery|cqeach|nc version=s pname=s));
  my @args = $unlock->args;
  $ct = ClearCase::Argv->new({autochomp=>1});
  my (@lbt, @oth, %vob);
  my $locvob = $ct->argv(qw(des -s vob:.))->stderr(0)->qx;
  foreach my $t (@args) {
    if ($ct->argv(qw(des -fmt), '%m\n', $t)->stderr(0)->qx eq 'label type') {
      my @et = grep s/^-> lbtype:(.*)@.*$/$1/,
	$ct->argv(qw(des -s -ahl), $eqhl, $t)->qx;
      if (@et) {
	if ($t =~ /lbtype:(.*)@(.*)$/) {
	  $t = $1; my $v = $2; $vob{$t} = $v; $vob{$et[0]} = $v;
	} else {
	  $t =~ s/^lbtype://;
	  $vob{$t} = $locvob; $vob{$et[0]} = $locvob;
	}
	push @lbt, $t, $et[0];
      } else {
	push @oth, $t;
      }
    } else {
      push @oth, $t;
    }
  }
  my $rc = @oth? $unlock->args(@oth)->system : 0;
  my ($fl, $loaded) = $ENV{FORCELOCK};
  for my $lt (@lbt) {
    my $v = $vob{$lt};
    if ($ct->argv(qw(lslock -s), "lbtype:$lt\@$v")->qx) {
      if ($unlock->args("lbtype:$lt\@$v")->stderr(0)->system) {
	if ($fl and !$loaded) {
	  my $fn = $fl; $fn =~ s%::%/%g; $fn .= '.pm';
	  require $fn;
	  $fl->import;
	  $loaded = 1;
	}
	if (!$fl or funlocklt($lt, $v)) {
	  warn Msg('E', "Could not unlock lbtype:$lt\@$v");
	  $rc = 1;
	}
      }
    } else {
      warn Msg('E', 'Object is not locked.');
      warn Msg('E', "Unable to unlock label type \"$lt\".");
      $rc = 1;
    }
  }
  exit $rc;
}

=item * MKLABEL

In case of a family type, apply also the equivalent incremental type.
The meaning of B<-replace> is affected: it concerns the equivalent fixed
type, and is implicit for the floating type (the one given as argument).

Preserve the support for the B<-up> flag from B<ClearCase::Wrapper::DSB>
and lift the restriction to using it only with B<-recurse>.

Added a B<-force> option which makes mostly sense in the case of applying
incremental labels. Without it, applying the floating label type will be
skipped if there has been errors while (incrementally) applying the
equivalent fixed one. Forcing the application may make sense if the errors
come from multiple application e.g. due to links, or in order to retry the
application after a first failure.
It may also be used to apply labels upwards even if recursive application
produced errors.

=cut

sub mklabel {
  use warnings;
  use strict;
  my %opt;
  GetOptions(\%opt, qw(up force));
  ClearCase::Argv->ipc(1);
  my $mkl = ClearCase::Argv->new(@ARGV);
  $mkl->parse(qw(replace|recurse|ci|cq|nc
		 version|c|cfile|select|type|name|config=s));
  my @opt = $mkl->opts();
  die Msg('E', 'Incompatible flags: up and config')
    if $opt{up} and grep /^-con/, @opt;
  my($lbtype, @elems) = $mkl->args;
  $lbtype =~ s/^lbtype://;
  $ct = ClearCase::Argv->new({autochomp=>1});
  my @et = grep s/^-> lbtype:(.*)@.*$/$1/,
    $ct->argv(qw(des -s -ahl), $eqhl, "lbtype:$lbtype")->qx;
  return 0 unless $opt{up} or @et;
  die Msg('E',
	  "Lock on label type \"$lbtype\" prevents operation \"make label\"")
    if $ct->argv(qw(lslock -s),"lbtype:$lbtype")->stderr(0)->qx;
  my ($ret, @rec) = 0;
  if (grep /^-r(ec)?$/, @opt) {
    if (@et) {
      @rec = $ct->argv(qw(ls -s -r -vob), @elems)->qx;
    } else {
      $mkl->syfail(1) unless $opt{force};
      $ret = $mkl->system;
    }
  }
  $mkl->opts(grep !/^-r(ec)?$/, @opt); # recurse handled already
  @opt = $mkl->opts;
  if ($opt{up}) {
    my $dsc = ClearCase::Argv->new({-autochomp=>1});
    require File::Basename;
    require File::Spec;
    File::Spec->VERSION(0.82);
    my $vroot;
    if ($^O eq 'cygwin') {
      $vroot = '/cygdrive/a'; #just for the length
    } elsif ($^O =~ /MSWin/) {
      $vroot = 'a:';
    } else {
       $vroot = $ct->argv(qw(pwv -root))->qx;
    }
    my %ancestors;
    for my $pname (@elems) {
      my $vobtag = $dsc->desc(['-s'], "vob:$pname")->qx;
      my $stop = length("$vroot$vobtag");
      for (my $dad = File::Basename::dirname(File::Spec->rel2abs($pname));
	   length($dad) >= $stop;
	   $dad = File::Basename::dirname($dad)) {
	$ancestors{$dad}++;
      }
    }
    if (@et) {
      push @elems, sort {$b cmp $a} keys %ancestors;
    } else {
      $ret |= $mkl->args($lbtype, @elems)->system;
      exit $ret;
    }
  }
  # Necessarily in the incremental type case
  push @elems, @rec;
  my @mod = grep {
    my $v = $_;
    $_ = (grep /^$lbtype$/, split/ /,
	  $ct->argv(qw(des -fmt), '%Nl', $v)->qx)? '' : $v;
  } @elems;
  exit $ret unless @mod;
  $ret = $mkl->args($et[0], @mod)->system if @et;
  exit $ret if $ret and !$opt{force};
  push @opt, '-rep' unless grep /^-rep/, @opt;
  $mkl->opts(@opt);
  $ret |= $mkl->args($lbtype, @mod)->system;
  exit $ret;
}

=back

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2007 IONA Technologies PLC (until v0.05),
2008-2009 Marc Girod (marc.girod@gmail.com) for later versions.
All rights reserved.
This Perl program is free software; you may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1), ClearCase::Wrapper, ClearCase::Wrapper::DSB, ClearCase::Argv

=cut
