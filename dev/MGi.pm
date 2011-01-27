package ClearCase::Wrapper::MGi;

$VERSION = '0.23';

use warnings;
use strict;
use vars qw($ct $eqhl $prhl $diat);
($eqhl, $prhl) = qw(EqInc PrevInc);

sub _Compareincs($$) {
  my ($t1, $t2) = @_;
  my ($p1, $M1, $m1, $s1) = pfxmajminsfx($t1);
  my ($p2, $M2, $m2, $s2) = pfxmajminsfx($t2);
  if (!(defined($p1) and defined($p2) and ($p1 eq $p2) and ($s1 eq $s2))) {
    warn Msg('W', "$t1 and $t2 not comparable\n");
    return 1;
  }
  return ($M1 <=> $M2 or (defined($m1) and defined($m2) and $m1 <=> $m2));
}
use AutoLoader 'AUTOLOAD';

#############################################################################
# Usage Message Extensions
#############################################################################
{
  local $^W = 0;
  no strict 'vars';

  my $z = $ARGV[0] || '';
  $checkin = "\n* [-dir|-rec|-all|-avobs] [-ok] [-diff [diff-opts]] [-revert]";
  $lsgenealogy =
    "$z [-short] [-all] [-obsolete] [-depth gen-depth] pname ...";
  $mkbrtype = "\n* [-archive]";
  $mklabel = "\n* [-up] [-force] [-over type [-all]]";
  $mklbtype = "\n* [-family] [-increment] [-archive]";
  $rmtype = "\n* [-family] [-increment]";
  $setcs = "\n* [-clone view-tag] [-expand] [-sync|-needed]";
}

#############################################################################
# Command Aliases
#############################################################################
*co             = *checkout;
*ci		= *checkin;
*lsgen		= *lsgenealogy;
*unco           = *uncheckout;

1;

__END__

## Internal service routines, undocumented.
sub _Samebranch($$) {		# same branch
  my ($cur, $prd) = @_;
  $cur =~ s:/[0-9]+$:/:; # Treat CHECKEDOUT as other branch
  $prd =~ s:/[0-9]+$:/:;
  return ($cur eq $prd);
}
sub _Sosbranch($$) {		# same or sub- branch
  my ($cur, $prd) = @_;
  $cur =~ s:/[0-9]+$:/:;
  $prd =~ s:/[0-9]+$:/:;
  return ($cur =~ qr(^$prd));
}
sub _Printparents {
  no warnings 'recursion';
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
  my $yes = ($opt{all} or (scalar(@p) != 1) or (scalar(@s) != 1)
	       or !_Samebranch($id, $s[0]) or !_Sosbranch($p[0], $id));
  if ($l or $yes) {
    if ($opt{short}) {
      if ($yes) {
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
      _Printparents($p, $gen, $seen, $ind);
    } else {
      printf("%${ind}s\[alternative path: ${p}\]\n", '')
	unless $opt{short};
    }
  }
}
sub _Findpredinstack($$) {
  my ($g, $stack) = @_;
  while ($$stack[-1]) {
    return $$stack[-1] if _Sosbranch($g, $$stack[-1]);
    pop @{$stack};
  }
  return 0;
}
sub _Setdepths {
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
    _Setdepths($p, $$gen{$id}{depth} + 1, $gen);
  }
}
sub _Checkcs {
  use File::Basename;
  use Cwd;
  my ($v) = @_;
  $ct = $ct->clone();
  $v =~ s/^(.*?)\@\@.*$/$1/;
  my $dest = dirname($v);
  $dest = '' if $dest eq '.';
  my $pwd = getcwd();
  $ct->argv('cd', $dest)->system if $dest;
  my @cs = grep /^\#\#:BranchOff: *root/, $ct->argv('catcs')->qx;
  $ct->argv('cd', $pwd)->system if $dest;
  return scalar @cs;
}
sub _Pbrtype {
  my ($pbrt, $bt) = @_;
  if (!defined($pbrt->{$bt})) {
    my $tc = $ct->argv('des', qw(-fmt %[type_constraint]p),
		       "brtype:$bt")->qx;
    $pbrt->{$bt} = ($tc =~ /one version per branch/);
  }
  return $pbrt->{$bt};
}
sub _Parsevtree($$$) {
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
    if (_Findpredinstack($g, \@stack)) {
      push @{ $gen{$g}{parents} }, $stack[-1];
      push @{ $gen{$stack[-1]}{children} }, $g;
    }
    push @stack, $g;
  }
  return %gen;
}
sub _Mkbco {
  my ($cmd, @cmt) = @_;
  my $rc = 0;
  my %pbrt = ();
  my $bt = $cmd->{bt};
  my @opts = $cmd->opts;
  if ($cmd->flag('nco')) { #mkbranch
    my @a = ($bt);
    push @a, $cmd->args;
    $cmd->args(@a);
    push @opts, @cmt;
    $cmd->opts(@opts);
    return $cmd->system;
  } elsif ($cmd->flag('branch')) { #co
    push @opts, @cmt;
    $cmd->opts(@opts);
    return $cmd->system;
  }
  die Msg('E', 'Element pathname required.') unless $cmd->args;
  foreach my $e ($cmd->args) {
    my $ver = $cmd->{ver};
    my $typ = $ct->argv(qw(des -fmt %m), $e)->qx;
    if ($typ !~ /(branch|version)$/) {
      warn Msg('W', "Not a vob object: $e");
      $rc = 1;
      next;
    }
    if ($ver) {
      $ver = "/$ver" unless $ver =~ m%^[\\/]%;
      $e =~ s%\@\@.*$%%;
      my $v = $e;
      $ver = "$v\@\@$ver";
    } elsif ($e =~ m%^(.*?)\@\@.*$%) {
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
    if ($bt and _Checkcs($e)) {
      my $main = ($ct->argv('lsvtree', $e)->qx)[0];
      $main =~ s%^[^@]*\@\@[\\/](.*)$%$1%;
      my $vob = $ct->argv('des', '-s', "vob:$e")->qx;
      my $re = _Pbrtype(\%pbrt, "$bt\@$vob") ?
	qr([\\/]${main}[\\/]$bt[\\/]\d+$) : qr([\\/]$bt[\\/]\d+$);
      if ($ver =~ m%$re%) {
	push @opts, @cmt, $e;
	$rc |= $ct->argv('co', @opts)->system;
      } else {
	my @mkbcopt = @cmt? @cmt : qw(-nc);
	if ($ct->argv('mkbranch', @mkbcopt, '-ver', "/$main/0", $bt, $e)
	      ->stderr(0)->system) {
	  $rc = 1;
	} else {
	  if ($ver !~ m%\@\@[\\/]${main}[\\/]0$%) {
	    my $lrc = $ct->argv('merge', '-to', $e, $ver)->stdout(0)->system;
	    unlink glob("$e.contrib*");
	    $rc |= $lrc;
	  }
	}
      }
    } else {
      my @args;
      push @args, @opts, @cmt;
      push @args, $bt if $bt;
      push @args, $e; # Ensure non empty array
      $rc |= $bt? $ct->argv('mkbranch', @args)->system
	: $ct->argv('co', @args)->system;
    }
  }
  return $rc;
}
sub _Ensuretypes($@) {
  my ($glb, @vob) = @_;
  my %cmt = ($eqhl => q(Equivalent increment),
	     $prhl => q(Previous increment in a type chain));
  my $silent = $ct->clone({stdout=>0, stderr=>0});
  my $die = $ct->clone({autofail=>1});
  my $gflg = $glb? '-glo' : '-ord';
  for my $t ($eqhl, $prhl) {
    for my $v (@vob) {
      my $t2 = "$t\@$v";
      $die->mkhltype([qw(-shared -c), $cmt{$t}, $gflg], $t2)->system
	if $silent->des(['-s'], "hltype:$t2")->system;
    }
  }
}
sub _Pfxmajminsfx($) {
  my $t = shift;
  if ($t =~ /^([\w.-]+[-_])(\d+)(?:\.(\d+))?(\@.*)?$/) {
    my $min = ($3 or '');
    my $sfx = ($4 or '');
    return ($1, $2, $min, $sfx);
  } else {
    warn Msg(
      'W', "$t doesn't match the pattern expected for an incremental type\n");
  }
}
sub _Nextinc($) {
  my $inc = shift;
  my ($pfx, $maj, $min, $sfx) = _Pfxmajminsfx($inc);
  return '' unless $pfx and $maj;
  my $count = defined($min)? $maj . q(.) . ++$min : ++$maj;
  return $pfx . $count . $sfx;
}
sub _Findnext($) {		# on input, the type exists
  my $c = shift;
  my @int = grep { s/^<- lbtype:(.*)$/$1/ }
    $ct->argv(qw(des -s -ahl), $prhl, "lbtype:$c")->qx;
  if (@int) {
    my @i = ();
    for (@int) {
      push @i, _Findnext($_);
    }
    return @i;
  } else {
    return ($c);
  }
}
sub _Findfreeinc($) {	   # on input, the values may or may not exist
  my ($nxt, %n) = shift;
  while (my ($k, $v) = each %{$nxt}) {
    while ($ct->argv(qw(des -s), "lbtype:$v")->stderr(0)->qx) { #exists
      my @cand = sort _Compareincs _Findnext($v);
      $v = _Nextinc($cand[$#cand]);
    }
    $n{$k} = $v;
  }
  while (my ($k, $v) = each %n) { $$nxt{$k} = $v }
}
sub _PreCi {
  use strict;
  use warnings;
  my ($ci, @arg) = @_;
  my $lsco = ClearCase::Argv->lsco([qw(-cview -s -d)])->stderr(1);
  my $res = $lsco->args(@arg)->qx;
  if (!$res or $res =~ /^cleartool: Error/m) {
    warn Msg('E', 'Unable to find checked out version for '
	       . join(', ', @arg) . "\n");
    return 0;
  }
  my $opt = $ci->{opthashre};
  return 1 unless $opt->{diff} || $opt->{revert};
  my $elem = $arg[0]; #Only one because of -cqe
  my $diff = $ci->clone->prog('diff');
  $ct = $ci->clone;
  $ct->autochomp(1);
  my $ver = $ct->argv(qw(des -fmt %En@@%Vn), $elem)->qx;
  $ver =~ s%\\%/%g;
  my $bra = $1 if $ver =~ m%^(.*?)/(?:\d+|CHECKEDOUT)$%;
  my %gen = _Parsevtree($elem, 1, $ver);
  my $p = $gen{$ver}{'parents'};
  my ($brp) = grep { m%^$bra/\d+$% } @{$p};
  $diff->optsDIFF(q(-serial), $diff->optsDIFF);
  $diff->args($brp? $brp : $p->[0], $elem);
  # Without -diff we only care about return code
  $diff->stdout(0) unless $opt->{diff};
  # With -revert, suppress msgs from typemgrs that don't do diffs
  $diff->stderr(0) if $opt->{revert};
  if ($diff->system('DIFF')) {
    return 1;
  } else {
    if ($opt->{revert}) { # unco instead of checkin
      _Unco(ClearCase::Argv->unco(['-rm'], $elem));
      return 0;
    } else { # -diff
      warn Msg('E', q(By default, won't create version with )
		 . q(data identical to predecessor.));
      return 0;
    }
  }
}
sub _Preemptcmt { #return the comments apart: e.g. mklbtype needs discrimination
  use File::Temp qw(tempfile);
  my ($cmd, $fn, $tst) = @_; #parsed, 3 groups: cquery|cqeach nc c|cfile=s
  use warnings;
  use strict;
  my @opts = $cmd->opts;
  my ($ret, @mod, @copt) = 0;
  my ($cqf, $ncf, $cf) =
    ($cmd->flag('cquery'), $cmd->flag('nc'), $cmd->flag('c'));
  if (!$cqf and !$ncf and !$cf) {
    if (defined $ENV{_CLEARCASE_PROFILE}) { #ClearCase::Argv shift
      if (open PRF, "<$ENV{_CLEARCASE_PROFILE}") {
	while (<PRF>) {
	  if (/^\s*(.*?)\s+(-\w+)/) {
	    my ($op, $fg) = ($1, $2);
	    if (($op eq ($cmd->prog())[1]) or ($op eq '*')) {
	      if ($fg eq '-nc') {
		$ncf = 1;
	      } elsif ($fg eq '-cqe') {
		$cqf = 1;
		push @opts, '-cqe';
	      } elsif ($fg eq '-cq') {
		$cqf = 1;
		push @opts, '-cq';
	      }
	      last;
	    }
	  }
	}
	close PRF;
      }
    }
    if (!$cqf and !$ncf and !$cf) {
      my $re =
	qr/c[io]|check(in|out)|mk(dir|elem|(at|br|el|hl|lb|tr)type|pool|vob)/;
      if (($cmd->prog())[1] =~ /^($re)$/) {
	$cqf = 1;
	push @opts, '-cqe';
      } else {
	$ncf = 1;
      }
    }
  }
  if ($ncf or $cf) {
    if ($ncf) {
      $cmd->opts(grep !/^-nc(?:omment)?$/,@opts);
      $ret = $fn->($cmd, qw(-nc));
    } else {
      my $skip = 0;
      for (@opts) {
	if ($skip) {
	  $skip = 0;
	  push @copt, $_;
	} else {
	  if (/^-c/) {
	    $skip = 1;
	    push @copt, $_;
	  } else {
	    push @mod, $_;
	  }
	}
      }
      $cmd->opts(@mod);
      $ret = $fn->($cmd, @copt);
    }
  } elsif ($cqf) {
    my $cqe = grep /^-cqe/, @opts;
    @opts = grep !/^-cq/, @opts;
    my @arg = $cmd->args;
    my $go = 1;
    while ($go) {
      $cmd->opts(@opts); #reset
      if ($cqe) {
	my $arg = shift @arg;
	$go = scalar @arg;
	next if $tst and !$tst->($cmd, $arg);
	$cmd->args($arg);
	print qq(Comments for "$arg":\n);
      } else {
	$go = 0;
	last if $tst and !$tst->($cmd, @arg); #None checked out
	print "Comment for all listed objects:\n";
      }
      my $cmt = '';
      while (<STDIN>) {
	last if /^\.$/;
	$cmt .= $_;
      }
      chomp $cmt;
      if ($cmt =~ /\n/) {
	my ($fh, $cfile) = tempfile();
	print $fh $cmt;
	close $fh;
	$ret |= $fn->($cmd, '-cfile', $cfile);
      } else {
	$ret |= $fn->($cmd, '-c', $cmt);
      }
    }
  }
  exit $ret;
}
sub _Unco {
  use warnings;
  use strict;
  my ($unco, $rc) = (shift, 0);
  $ct = ClearCase::Argv->new({autochomp=>1});
  for my $arg ($unco->args) { # Already sorted if several
    my $b0 = $ct->argv(qw(ls -s -d), $arg)->qx;
    $rc |= $unco->args($arg)->system;
    if ($b0 =~ s%^(.*)[\\/]CHECKEDOUT$%$1%) {
      opendir BR, $b0 or next;
      my @f = grep !/^\.\.?$/, readdir BR;
      closedir BR;
      $ct->argv(qw(rmbranch -f), $b0)->system
	if (scalar @f == 2) and $f[0] eq '0' and $f[1] eq 'LATEST';
    }
  }
  return $rc;
}
sub _Yesno {
  my ($cmd, $fn, $yn, $test, $errmsg) = @_;
  use warnings;
  use strict;
  my $ret = 0;
  my @opts = $cmd->opts;
  for my $arg ($cmd->args) {
    $cmd->opts(@opts); #reset
    if ($test) {
      my $res = $test->($arg);
      if (!$res) {
	warn ($res or Msg('E', $errmsg . '"' . $arg . "\".\n"));
	next;
      } elsif ($res == -1) { # Skip interactive part
	$ret |= $fn->($cmd);
	next;
      }
    }
    printf $yn->{format}, $arg;
    my $ans = <STDIN>; chomp $ans; $ans = lc($ans);
    $ans = $yn->{default} unless $ans;
    while ($ans !~ $yn->{valid}) {
      print $yn->{instruct};
      $ans = <STDIN>; chomp $ans; $ans = lc($ans);
      $ans = $yn->{default} unless $ans;
    }
    if ($yn->{opt}->{$ans}) {
      $cmd->opts(@opts, $yn->{opt}->{$ans});
      $cmd->args($arg);
      $ret |= $fn->($cmd);
    } else {
      $ret = 1;
    }
  }
  exit $ret;
}
sub _Checkin {
  use strict;
  use warnings;
  my ($ci, @cmt) = @_;
  $ci->opts($ci->opts, @cmt);
  if ($ci->flag('from') and !$ci->flag('keep')) {
    my %kr = (yes => '-keep', no => '-rm');
    my %yn = (
      format   => q(Save private copy of "%s"?  [yes] ),
      default  => q(yes),
      valid    => qr/yes|no/,
      instruct => "Please answer with one of the following: yes, no\n",
      opt      => \%kr,
    );
    my $lsco = ClearCase::Argv->lsco([qw(-cview -s)]);
    $lsco->stderr(1);
    my $ok = sub { return $lsco->args(shift)->qx? 1:0 };
    my $err = 'Unable to find checked out version for ';
    my $run = sub { my $ci = shift; $ci->system };
    _Yesno($ci, $run, \%yn, $ok, $err); #only one arg: may exit
  } else {
    return $ci->system;
  }
}
# Input: lbtype type, either floating 'family' type, or fixed
# 'incremental'.
# Output: ordered list of short type names, as chained.
sub _EqLbTypeList {
  use strict;
  use warnings;
  my $top = shift;
  return unless $top;
  $top = "lbtype:$top" unless $top =~ /^lbtype:/;
  my $ct = ClearCase::Argv->new({autochomp=>1});
  my ($eq) = grep s/^-> (.*)$/$1/,
    $ct->argv(qw(des -s -ahl), $eqhl, $top)->qx;
  $_ = $eq? $eq : $top;
  my @list;
  do {
    push @list, $1 if /^lbtype:(.*?)(@.*)?$/;
    ($_) = grep s/^-> (.*)$/$1/,
      $ct->argv(qw(des -s -ahl), $prhl, $_)->qx;
  } while ($_);
  return @list;
}

=head1 NAME

ClearCase::Wrapper::MGi - Support for an alternative to UCM.

=head1 SYNOPSIS

This is an C<overlay module> for B<ClearCase::Wrapper> containing Marc
Girod's non-standard extensions. See C<perldoc ClearCase::Wrapper> (by
David Boyce) for more details.

The alternative to UCM consists in a novel branching model, and a concept
of incremental types.

=head1 CLEARTOOL EXTENSIONS

=over 2

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

=over 1

=item B<-all>

Show 'uninteresting' versions, otherwise skipped:

=over 1

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
    my %gen = _Parsevtree($ele, $opt{obsolete}, $ver);
    _Setdepths($ver, 0, \%gen);
    my %seen = ();
    _Printparents($ver, \%gen, \%seen, 0);
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
branch, and cascading branches indefinitely.  The logical version tree
is restituted by navigating the merge arrows, to find all the direct
or indirect contributors.

Flag:

=over 1

=item B<-ver/sion>

Ignored under a I<BranchOff> config spec,
but the version specified in the pname is anyway obeyed,
as a branch may always be spawn.

=back

=cut

sub checkout {
  map { $_ = readlink if -l && defined readlink } @ARGV[1..$#ARGV];
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
  ClearCase::Argv->ipc(1) unless ClearCase::Argv->ctcmd(); #set by the user
  $ct = ClearCase::Argv->new({autochomp=>1});
  my $co = ClearCase::Argv->new(@ARGV);
  $co->parse(qw(reserved unreserved nmaster out=s ndata ptime nwarn
		version branch=s query nquery usehijack
		cquery|cqeach nc c|cfile=s));
  my ($unsup) = grep /^-/, $co->args;
  die Msg('E', qq(Unrecognized option "$unsup")) if $unsup;
  if (MSWIN) {
    my @args = $co->args;
    map { $_ = glob($_) } @args;
    $co->args(@args);
  }
  _Preemptcmt($co, \&_Mkbco);
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
  map { $_ = readlink if -l && defined readlink } @ARGV[1..$#ARGV];
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
  ClearCase::Argv->ipc(1) unless ClearCase::Argv->ctcmd();
  $ct = ClearCase::Argv->new({autochomp=>1});
  my $ver;
  GetOptions("version=s" => \$ver);
  my $mkbranch = ClearCase::Argv->new(@ARGV);
  $mkbranch->parse(qw(nwarn nco ptime cquery|cqeach nc c|cfile=s));
  my @args = $mkbranch->args;
  my $bt = shift @args;
  map { $_ = glob($_) } @args if MSWIN;
  $bt =~ s/^brtype:(.*)$/$1/;
  my %v;
  if ($bt =~ /\@/) {
    $v{''} = 1;
  } else {
    for my $a (@args) {
      $v{$ct->argv(qw(des -fmt), '@%n', "vob:$a")->stderr(0)->qx}++;
    }
  }
  for (keys %v) {
    die "\n" if $ct->argv(qw(des -s), "brtype:$bt$_")->stdout(0)->system;
  }
  $mkbranch->args(@args);
  $mkbranch->{bt} = $bt;
  $mkbranch->{ver} = $ver;
  _Preemptcmt($mkbranch, \&_Mkbco);
}

=item * DIFF

Evaluate the predecessor from the genealogy, i.e. take into account merges on
an equal basis as parents on the same physical branch.
In case there are multiple parents, consider the one on the same branch as
'more equal than the others' (least surprise principle).

Preserve the (Wrapper default) assumption of a B<-pred> flag, if only one
argument is given.

=cut

sub diff {
  for (@ARGV[1..$#ARGV]) {
    $_ = readlink if -l && defined readlink;
  }
  push(@ARGV, qw(-dir)) if @ARGV == 1;
  my $limit = 0;
  if (my @num = grep /^-\d+$/, @ARGV) {
    @ARGV = grep !/^-\d+$/, @ARGV;
    die Msg('E', "incompatible flags: @num") if @num > 1;
    $limit = -int($num[0]);
  }
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
  my $rc = 0;
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
    my %gen = _Parsevtree($ele, 1, $ver);
    my $p = $gen{$ver}{'parents'};
    $p = $gen{$p->[0]}{'parents'} while $p and $limit--;
    if (!$p) {
      warn Msg('E', "No predecessor version to compare to: $e");
      $rc = 1;
      next;
    }
    my ($brp) = grep { m%^$bra/\d+$% } @{$p};
    $ver = $ele if $ver =~ m%/CHECKEDOUT$%;
    $rc |= $diff->args($brp? $brp : $p->[0], $ver)->system;
  }
  exit $rc;
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
  $unco->parse(qw(keep|rm cact cwork));
  $unco->optset('IGNORE');
  $unco->parseIGNORE(qw(c|cfile=s cquery|cqeach nc));
  $unco->args(sort {$b cmp $a} AutoCheckedOut($opt{ok}, $unco->args));
  if ($unco->flag('keep')) {
    exit _Unco($unco);
  } else {
    my %kr = (yes => '-keep', no => '-rm');
    my %yn = (
      format   => q(Save private copy of "%s"?  [yes] ),
      default  => q(yes),
      valid    => qr/yes|no/,
      instruct => "Please answer with one of the following: yes, no\n",
      opt      => \%kr,
    );
    my $lsco = ClearCase::Argv->lsco([qw(-cview -s)]);
    $lsco->stderr(1);
    my $dm = ClearCase::Argv->des([qw(-fmt %m)]);
    $dm->stderr(1);
    my $ok = sub {
      my $e = shift;
      return -1 if $dm->args($e)->qx =~ /^directory/;
      return $lsco->args($e)->qx? 1:0;
    };
    my $err = 'Unable to find checked out version for ';
    _Yesno($unco, \&_Unco, \%yn, $ok, $err);
  }
}

=item * MKLBTYPE

Extension: families of types, with a chain of fixed types, linked in a
succession, and one floating type, convenient for use in config specs.
One application is incremental types, applied only to modified versions,
allowing however to simulate full baselines.
Such a functionality is implemented as part of UCM, for fixed types,
and I<magic> config specs. The wrapper offers thus something similar
on base ClearCase.

This wrapper also includes a modified version of the extension for
global types originally proposed in C<ClearCase::Wrapper::DSB>. In the
realm of an admin vob, types are created global by default. This
implementation makes the feature configurable, via a
C<$ClearCase::Wrapper::MGi::global> variable set in
C<.clearcase_profile.pl>, so that a user is not forced to obey an
injonction from (possibly other site) administrators.

The current baseline is embodied with floating labels, which are moved
over successive versions. The first pair of a floating and a fixed
type is created with the B<-fam/ily> flag. Further fixed types are
created with the B<-inc/rement> flag.

Types forming a family are related with hyperlinks of two types:

=over 1

=item B<EqInc>

Equivalent incremental fixed type. A I<fixed> (i.e. which by
convention, will not been I<moved>), I<sparse> (i.e. applied only to
changes) type. This helps to mark the history of application of the
I<floating> type, which is also a I<full> one, for reproducibility
purposes.

=item B<PrevInc>

Previous incremental fixed type.

=back

Attributes are created of a per label family type, and are used to
mark the deletion of labels applied at a previous increment.  The
attribute type for family lbtype I<XXX> is I<RmXXX>, and the value is
the numeric (treated as I<real>) value of the increment.

Flags:

=over 1

=item B<-fam/ily>

Create two label types, linked with an B<EqInc> hyperlink.
The first, given as argument, will be considered as an alias for successive
increments of the second. It is the I<family> type.
The name of the initial incremental type is this of the I<family> type, with
a suffix of I<_1.00>.

Also create a I<RmLBTYPE> attribute type to record removals of labels.

=item B<-inc/rement>

Create a new increment of an existing label type family, given as argument.
This new type will take the place of the previous increment, as the
destination of the B<EqInc> hyperlink on the I<family> type.
It will have a B<PrevInc> hyperlink pointing to the previous increment in
the family.

=item B<-arc/hive>

Rename the current type to an I<archive> value (name as prefix, and a
numeral suffix. Initial value: I<-001>), create a new type, and make the
archived one its predecessor, with a B<PrevInc> hyperlink.
Comments go to the type being archived.

The implementation is largely shared with I<mkbrtype>.

For label types, the newly created type is hidden away (with a suffix
of I<_0>) and locked. It is being restored the next time C<mklbtype -fam>
is given for the same name.

=item B<-glo/bal>

Support for global family types is preliminary.

=back

=cut

sub _GenMkTypeSub {
  use strict;
  use warnings;
  my ($type, $Name, $name) = @_;
  return sub {
    my ($ntype, @cmt) = @_;
    my $rep = $ntype->{rep};
    $ct = new ClearCase::Argv({autochomp=>1});
    my @args = $ntype->args;
    my %opt = %{$ntype->{fopts}};
    my $silent = $ct->clone({stdout=>0});
    if ((grep /^-glo/, $ntype->opts) or ($ntype->flag('global') and !%opt)) {
      push @cmt, '-rep' if $rep;
      $ntype->opts(@cmt, $ntype->opts);
      return $ntype->system
    }
    my (%vob, $unkvob);
    /\@(.*)$/? $vob{$1}++ : $vob{'.'}++ for @args;
    my @vob = keys %vob;
    for (@vob) { #Diagnose all the errors, even if one is enough to fail
      $unkvob++ if $silent->argv(qw(des -s), "vob:$_")->system;
    }
    return 1 if $unkvob;
    if ($ClearCase::Wrapper::MGi::global and !$ntype->flag('global')) {
      my %avob;
      for my $v (@vob) {
	my ($hl) = grep s/^-> (?:vob:)(.*)$/$1/,
	  $ct->desc([qw(-s -ahl AdminVOB), "vob:$v"])->qx;
	$avob{$v} = $hl if $hl;
      }
      if (scalar keys %avob) {
	$ntype->opts('-global', $ntype->opts);
	for (@vob) {	   #Fix the vobs, to ensure the metadata types
	  $_ = $avob{$_} if $avob{$_};
	}
	for (@args) { #Fix the types, to create them in the admin vob(s)
	  $_ = $1 . $avob{$2} if /^(.*\@)(.*)$/ and $avob{$2};
	}
	warn Msg('W', "making global type(s) @args");
      }
    }
    if (%opt) {
      map { s/^${type}:(.*)$/$1/ } @args;
      my @a = @args;
      _Ensuretypes((grep /^-glo/, $ntype->opts), @vob);
      if ($rep) {
	@args =
	  grep { $_ = $ct->argv(qw(des -fmt), '%Xn', "$type:$_")->qx } @args;
	exit 1 unless @args;
	if ($opt{family}) {
	  @a = ();
	  my $gflg = (grep /^-glo/, $ntype->opts)? '-glo' : '-ord';
	  foreach my $t (@args) {
	    if ($ct->argv(qw(des -s -ahl), $eqhl, $t)->stderr(0)->qx) {
	      warn Msg('E', "$t is already a family type\n");
	      if ($t =~ s/^lbtype:(.*)$/$1/) {
		my $att = "Rm$t";
		$ct->argv(qw(mkattype -vtype real -c), q(Deleted in increment),
			  $gflg, "$att")->stderr(0)->system;
	      }
	    } else {
	      push @a, $t;
	    }
	  }
	  exit 1 unless @a;
	  my %pair = ();
	  foreach (@a) {
	    if (/^$type:(.*)(@.*)$/) {
	      $pair{"$1$2"} = "${1}_1.00$2";
	    }
	  }
	  _Findfreeinc(\%pair);
	  $ntype->args(values %pair);
	  $ntype->opts(@cmt, $ntype->opts);
	  $ntype->system;
	  map {
	    if (defined($pair{$_})) {
	      my $inc = "$type:$pair{$_}";
	      $silent->argv('mkhlink', $eqhl, "$type:$_", $inc)->system;
	      if ($type eq 'lbtype') {
		my $att = "Rm$_";
		$ct->argv(qw(mkattype -vtype real -c), q(Deleted in increment),
			  $gflg, "$att")->stderr(0)->system;
	      }
	    }
	  } keys %pair;
	} elsif ($opt{archive}) {
	  my $rc = 0;
	  foreach my $t (@args) {
	    my ($pfx, $vob) = $t =~ /^$type:(.*)(@.*)$/;
	    my ($prev) = grep s/^-> $type:(.*)@.*/$1/,
	      $ct->argv(qw(des -s -ahl), $prhl, $t)->stderr(0)->qx;
	    my $arc;
	    if ($prev) {
	      if (my ($pfx, $nr) = $prev =~ /^(.*-)(\d+)$/) {
		$arc = $pfx . $nr++;
	      } else {
		$arc = $prev . '-001';
	      }
	    } else {
	      $arc = $pfx . '-001';
	    }
	    ($pfx, my $nr) = $arc =~ /^(.*-)(\d+)$/;
	    while ($ct->argv(qw(des -s), "$type:${arc}$vob")->stderr(0)->qx) {
	      $arc = $pfx . $nr++;
	    }
	    if ($ct->argv('rename', $t, $arc)->system) {
	      $rc = 1;
	      next;
	    }
	    $ntype->args($t);
	    $ntype->opts('-nc', $ntype->opts);
	    $ntype->system;
	    my $at = "$type:${arc}$vob";
	    $silent->argv('mkhlink', $prhl, $t, $at)->system;
	    if ($type eq 'lbtype') {
	      my $t0 = $t;
	      $t0 =~ s/^lbtype:(.*)(@.*)$/lbtype:$ {1}_0$2/;
	      $ct->argv('rename', $t, $t0)->system;
	      $ct->argv('lock', $t0)->system;
	    }
	    $ct->argv('chevent', @cmt, $at)->stdout(0)->system
	      unless $cmt[0] and $cmt[0] =~ /^-nc/;
	  }
	  exit $rc;
	} else {		# increment
	  die Msg('E', "Incompatible flags: replace and incremental");
	}
      } else {
	if ($opt{family}) {
	  @a = @args;
	  map { $_ = "$type:$_" } @a;
	  die Msg('E', "Some types already exist among @args")
	    unless $silent->argv(qw(des -s), @a)->stderr(0)->system;
	  my (%pair, @skip) = ();
	TYPE: foreach my $t (@args) {
	    if ($t =~ /^(.*?)(@.*)?$/) {
	      my ($pfx, $sfx) = ($1, $2?$2:'');
	      if ($type eq 'lbtype') {
		my $t0 = "lbtype:${pfx}_0$sfx";
		if ($ct->argv(qw(des -s), $t0)->stderr(0)->qx) {
		  $ct->argv('unlock', $t0)->system;
		  $ct->argv('rename', $t0, "lbtype:$t")->stderr(0)->system
		    and die Msg('E', qq(Failed to restore "$t0" into "$t".));
		  push @skip, $t;
		  if (my ($p) = grep s/^-> (.*)$/$1/,
		      $ct->des([qw(-s -ahl), $prhl], "lbtype:$t")->qx) {
		    ($pair{$t}) = grep s/^-> lbtype:(.*)$/$1/,
		      $ct->des([qw(-s -ahl), $eqhl], $p)->qx;
		  } else {
		    $pair{$t} = "${pfx}_1.00$sfx";
		  }
		} else {
		  $pair{$t} = "${pfx}_1.00$sfx";
		}
	      } else {
		$pair{$t} = "${pfx}_1.00$sfx";
	      }
	    }
	  }
	  my @opts = $ntype->opts();
	  if (%pair) {
	    _Findfreeinc(\%pair);
	    $ntype->args(values %pair);
	    $ntype->opts(@cmt, @opts);
	    $ntype->system;
	  }
	  if (@skip) {
	    my $sk = '(' . join('|', @skip) . ')';
	    @a = grep !/$sk/, @args;
	  } else {
	    @a = @args;
	  }
	  if (@a) {
	    $ntype->args(@a);
	    $ntype->opts('-nc', @opts);
	    $ntype->system;
	  }
	  my $gflg = (grep /^-glo/, $ntype->opts)? '-glo' : '-ord';
	  for (keys %pair) {
	    if (defined($pair{$_})) {
	      my $inc = "$type:$pair{$_}";
	      $silent->argv('mkhlink', $eqhl, "$type:$_", $inc)->system;
	      next if $type eq 'brtype';
	      my $att = "Rm$_";
	      $ct->argv(qw(mkattype -vtype real -c), q(Deleted in increment),
			$gflg, "$att")->stderr(0)->system;
	    }
	  }
	} elsif ($opt{increment}) { # increment
	  $ntype->opts(@cmt, $ntype->opts);
	  my $lct = ClearCase::Argv->new(); #Not autochomp
	  my ($fl, $loaded) = $ENV{FORCELOCK};
	INCT: for my $t (@args) {
	    my ($pt, $lck) = "$type:$t";
	    if (!$ct->argv(qw(des -s), $pt)->stderr(0)->qx) {
	      warn Msg('E', qq($Name type not found: "$t"));
	      next;
	    }
	    my ($pair) = grep s/^\s*(.*) -> $type:(.*)\@(.*)$/$1,$2,$3/,
	      $ct->argv(qw(des -l -ahl), $eqhl, $pt)->stderr(0)->qx;
	    my ($hlk, $prev, $vob) = split /,/, $pair if $pair;
	    if (!$prev) {
	      warn Msg('E', "Not a family type: $t");
	      next INCT;
	    }
	    my ($t1) = $t =~ /^(.*?)(@|$)/;
	    for my $l ($t1, $prev) {
	      if ($ct->argv(qw(lslock -s), "lbtype:$l\@$vob")->stderr(0)->qx) {
		$lck = 1;  #remember to lock the equivalent fixed type
		#This should happen as vob owner, to retain the timestamp
		my @out =
		  $lct->argv('unlock', "lbtype:$l\@$vob")->stderr(1)->qx;
		if (grep /^cleartool: Error/, @out) {
		  if ($fl and !$loaded) {
		    my $fn = $fl; $fn =~ s%::%/%g; $fn .= '.pm';
		    require $fn;
		    $fl->import;
		    $loaded = 1;
		  }
		  if (!$fl) {
		    print @out;
		    next INCT;
		  } elsif (funlocklt($l, $vob)) {
		    next INCT;
		  }
		} else {
		  print @out;
		}
	      }
	    }
	    if ($prev =~ /^(.*)_(\d+)(?:\.(\d+))?$/) {
	      my ($base, $maj, $min, $new) = ($1, $2, $3);
	      my $ext = ($t =~ /^.*(@.*)$/)? $1 : '';
	      my $p1 = $prev . $ext;
	      do {
		$new = "${base}_" .
		  (defined($min)? $maj . '.' . ++$min : ++$maj);
		$new .= $ext;
	      } while ($ct->argv(qw(des -s), "$type:$new")->stderr(0)->qx);
	      $ntype->args($new)->system and exit 1;
	      $silent->argv('rmhlink', $hlk)->system;
	      $silent->argv(qw(mkhlink -nc), $eqhl,
			    "$type:$t", "$type:$new")->system;
	      $silent->argv(qw(mkhlink -nc), $prhl,
			    "$type:$new", "$type:$p1")->system;
	      if ($lck) {
		my @out =
		  $lct->argv('lock', "lbtype:$prev\@$vob")->stderr(1)->qx;
		if ($fl and grep /^cleartool: Error/, @out) {
		  flocklt($prev, $vob); # loaded while unlocking
		} else {
		  print @out;
		}
	      }
	    } else {
	      warn Msg('W',qq(Previous increment non suitable in $t: "$prev"));
	    }
	  }
	}
      }
      exit 0;
    } else {			# non inc/arc/fam
      if ($rep) {
	$ntype->opts(@cmt, '-replace', $ntype->opts);
	map { $_ = "$type:$_" unless /^$type:/ } @args;
	my @a = $ct->argv(qw(des -s), @args)->stderr(0)->qx;
	if (@a) {
	  map { $_ = "$type:$_" } @a;
	  my @link = grep s/^\s*(.*) -> .*$/$1/,
	    $ct->argv(qw(des -l -ahl), "$eqhl,$prhl", @a)->qx;
	  $ct->argv('rmhlink', @link)->system;
	} else {
	  foreach (@args) {
	    s/^$type://;
	    warn Msg('E', qq($Name type not found: "$_".));
	  }
	  exit 1;
	}
      } else {
	$ntype->args(@args);
	$ntype->opts(@cmt, $ntype->opts);
	return $ntype->system;
      }
    }
  };
}
sub _GenExTypeSub {
  use strict;
  use warnings;
  my $type = shift;
  return sub {
    my ($mkt, $arg) = @_;
    $arg = "$type:$arg" unless $arg =~ /^$type:/;
    # Maybe need to check that non locked?
    return ClearCase::Argv->des('-s', $arg)->stdout(0)->system? 0 : 1;
  };
}
sub mklbtype {
  use strict;
  use warnings;
  my (%opt, $rep);
  GetOptions(\%opt, qw(family increment archive));
  GetOptions('replace' => \$rep);
  die Msg('E', 'Incompatible options: family increment archive')
    if keys %opt > 1;
  ClearCase::Argv->ipc(1);
  my $ntype = ClearCase::Argv->new(@ARGV);
  $ntype->parse(qw(global|ordinary vpelement|vpbranch|vpversion
		   pbranch|shared gt|ge|lt|le|enum|default|vtype=s
		   cquery|cqeach nc c|cfile=s));
  if (!$ntype->args) {
    warn Msg('E', 'Type name required.');
    @ARGV = qw(help mklbtype);
    ClearCase::Wrapper->help();
    return 1;
  }
  $ntype->{fopts} = \%opt;
  $ntype->{rep} = $opt{archive}? 1 : $rep;
  my $tst = $ntype->{rep}? _GenExTypeSub('lbtype') : 0;
  _Preemptcmt($ntype, _GenMkTypeSub(qw(lbtype Label label)), $tst);
}

=item * MKBRTYPE

Extension: archive a brtype away, in order to avoid having to modify
config specs using it (rationale: config specs are not versioned, so
they'd rather be stable). Also, starting new branches from the I<main>
one (whatever its real type) makes it easier to roll back changes if
need-be, branch off an earlier version, and bring back again the
changes rolled back at some later stage, after the problems have been
fixed.

The implementation is largely shared with I<mklbtype>.
See its documentation for the B<PrevInc> hyperlink type.

=over 1

=item B<-arc/hive>

Rename the current type to an I<archive> value (name as prefix, and a
numeral suffix. Initial value: I<-001>), create a new type, and make the
archived one its predecessor, with a B<PrevInc> hyperlink.
Comments go to the type being archived.

=item B<-glo/bal>

Global types (in an Admin vob or not) are currently not supported for
archiving.

=back

=cut

sub mkbrtype {
  use strict;
  use warnings;
  my (%opt, $rep);
  GetOptions(\%opt, q(archive));
  GetOptions('replace' => \$rep);
  die Msg('E', 'Incompatible options: global types cannot be archived')
    if %opt and grep /^-glo/, @ARGV;
  ClearCase::Argv->ipc(1);
  my $ntype = ClearCase::Argv->new(@ARGV);
  $ntype->parse(qw(global|ordinary acquire pbranch
		   cquery|cqeach nc c|cfile=s));
  $ntype->{fopts} = \%opt;
  $ntype->{rep} = $opt{archive}? 1 : $rep;
  my $tst = $ntype->{rep}? _GenExTypeSub('brtype') : 0;
  _Preemptcmt($ntype, _GenMkTypeSub(qw(brtype Branch branch)), $tst);
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
  $lock->parse(qw(c|cfile=s cquery|cqeach nc pname=s obsolete replace));
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
	     (!$currlock or $opt{iflocked} or $lock->flag('replace'))) {
    $lock->opts($lock->opts, '-nusers', ($nusers or $opt{allow}));
  }
  if ($currlock and !$lock->flag('replace')) {
    if ($opt{allow} or $opt{deny}) {
      $lock->opts($lock->opts, '-replace')
    } else {
      die Msg('E', 'Object is already locked.');
    }
  }
  my @args = $lock->args;
  $ct = ClearCase::Argv->new({autochomp=>1});
  my (@lbt, @oth, %vob);
  my $locvob = $ct->argv(qw(des -s vob:.))->stderr(0)->qx;
  foreach my $t (@args) {
    if ($ct->argv(qw(des -fmt), '%m\n', $t)->stderr(0)->qx eq 'label type') {
      my ($t1,$v) = $t;
      if ($t =~ /lbtype:(.*)@(.*)$/) {
	$t = $1; $v = $2;
      } else {
	$t =~ s/^lbtype://;
	$v = $locvob;
      }
      $vob{$t} = $v;
      push @lbt, $t;
      my @et = grep s/^-> lbtype:(.*)@.*$/$1/,
	$ct->argv(qw(des -s -ahl), $eqhl, $t1)->qx;
      if (@et) {
	my ($e, $p) = ($et[0], '');
	$vob{$e} = $vob{$t};
	push @lbt, $e;
	my @pt = grep s/^-> lbtype:(.*)@.*$/$1/,
	  $ct->argv(qw(des -s -ahl), $prhl, "lbtype:$e\@$v")->qx;
	if (@pt) {
	  $p = $pt[0];
	  if (!$ct->argv(qw(lslock -s), "lbtype:$p\@$v")->stderr(0)->qx) {
	    push @lbt, $p;
	    $vob{$p} = $v;
	  }
	}
      }
    } else {
      push @oth, $t;
    }
  }
  my $rc = @oth? $lock->args(@oth)->system : 0;
  my ($fl, $loaded) = $ENV{FORCELOCK};
  for my $lt (@lbt) {
    my $v = $vob{$lt};
    my @out = $lock->args("lbtype:$lt\@$v")->stderr(1)->qx;
    if (grep /^cleartool: Error/, @out) {
      if ($fl and !$loaded) {
	my $fn = $fl; $fn =~ s%::%/%g; $fn .= '.pm';
	require $fn;
	$fl->import;
	$loaded = 1;
      }
      if (!$fl) {
	print @out;
	$rc = 1;
      } elsif (flocklt($lt, $v, $lock->flag('replace'),
		       ($nusers or $opt{allow}))) {
	$rc = 1;
      }
    } else {
      print @out;
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
  $unlock->parse(qw(c|cfile=s cquery|cqeach nc version=s pname=s));
  my @args = $unlock->args;
  $ct = ClearCase::Argv->new({autochomp=>1});
  my (@lbt, @oth, %vob);
  my $locvob = $ct->argv(qw(des -s vob:.))->stderr(0)->qx;
  foreach my $t (@args) {
    if ($ct->argv(qw(des -fmt), '%m\n', $t)->stderr(0)->qx eq 'label type') {
      my $t1 = $t;
      if ($t =~ /lbtype:(.*)@(.*)$/) {
	$t = $1; $vob{$t} = $2;
      } else {
	$t =~ s/^lbtype://;
	$vob{$t} = $locvob;
      }
      push @lbt, $t;
      my @et = grep s/^-> lbtype:(.*)@.*$/$1/,
	$ct->argv(qw(des -s -ahl), $eqhl, $t1)->qx;
      if (@et) {
	push @lbt, $et[0];
	$vob{$et[0]} = $vob{$t};
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
      my @out = $unlock->args("lbtype:$lt\@$v")->stderr(1)->qx;
      if (grep /^cleartool: Error/, @out) {
	if ($fl and !$loaded) {
	  my $fn = $fl; $fn =~ s%::%/%g; $fn .= '.pm';
	  require $fn;
	  $fl->import;
	  $loaded = 1;
	}
	if (!$fl) {
	  print @out;
	  $rc = 1;
	} elsif (funlocklt($lt, $v)) {
	  $rc = 1;
	}
      } else {
	print @out;
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

Extension: B<-over> takes either a label or a branch type. In either case,
the labels will be applied over the result of a find command run on the
unique version argument, and looking for versions matching respectively
B<lbtype(xxx)> or <version(.../xxx/LATEST)> queries, and B<!lbtype(lb)> (with
I<xxx> the B<-over>, and I<lb> the main label type parameter.
Internally B<-over> performs a B<find>. This one depends by default on the
current config spec, with the result that it is not guaranteed to reach all
the versions specified, at least in the first pass. One may thus use an B<-all>
option which will be passed to the B<find>.
The B<-over> option doesn't require an element argument (default: current
directory). With the B<-all> option, it uses one if given, as a filter.

=cut

sub mklabel {
  use warnings;
  use strict;
  my %opt;
  GetOptions(\%opt, qw(up force over=s all));
  ClearCase::Argv->ipc(1);
  my $mkl = ClearCase::Argv->new(@ARGV);
  $mkl->parse(qw(replace|recurse|follow|ci|cq|nc
		 version=s c|cfile|select|type|name|config=s));
  my @opt = $mkl->opts();
  die Msg('E', 'all is only supported in conjunction with over')
    if $opt{all} and !$opt{over};
  die Msg('E', 'Incompatible flags: up and config')
    if $opt{up} and grep /^-con/, @opt;
  die Msg('E', 'Incompatible flags: recurse and over')
    if $opt{over} and grep /^-r(ec|$)/, @opt;
  die Msg('E', 'Incompatible flags: up and over') if $opt{up} and $opt{over};
  my($lbtype, @elems) = $mkl->args;
  die Msg('E', 'Only one version argument with the over flag')
    if $opt{over} and scalar @elems > 1;
  die Msg('E', 'Label type required') unless $lbtype;
  $lbtype =~ s/^lbtype://;
  $ct = ClearCase::Argv->new({autochomp=>1});
  my (%vb, @lt);
  for my $e (@elems?@elems:qw(.)) {
    my $v = $ct->argv(qw(des -s), "vob:$e")->stderr(0)->qx;
    $vb{$v}++ if $v;
  }
  if ($lbtype =~ /@/) {
    push @lt, $lbtype;
  } else {
    push @lt, "$lbtype\@$_" for keys %vb;
  }
  my @lt1 = @lt;
  my @et = grep s/^-> lbtype:(.*)@.*$/$1/,
    map { $ct->argv(qw(des -s -ahl), $eqhl, "lbtype:$_")->qx } @lt1;
  return 0 unless $opt{up} or $opt{over} or @et;
  my $fail = $ct->clone({autofail=>1});
  $fail->argv(qw(des -s), @elems)->stdout(0)->system
    unless $opt{over} and !@elems; #eq fixed => 1 failure fails all
  die Msg('E', "Only one vob supported for family types") if @et > 1;
  map {
    die Msg('E', qq(Lock on label type "$_" prevents operation "make label"))
      if $ct->argv(qw(lslock -s),"lbtype:$_")->stderr(0)->qx
    } @lt;
  my ($ret, @rec, @mod) = 0;
  if (grep /^-r(ec|$)/, @opt) {
    if (@et) {
      #The -vob_only flag would hide the checkout info for files
      @rec = grep m%@@[/\\]%, $ct->argv(qw(ls -s -r), @elems)->qx;
    } else {
      $mkl->syfail(1) unless $opt{force};
      $ret = $mkl->system;
    }
  } elsif ($opt{over}) {
    my ($t, $ver, $lb) = ($opt{over}, $elems[0]);
    die Msg('E', 'The -over flag requires a local type argument')
      if !$t or $t =~ /\@/;
    my $base = $ver || '.';
    my $vob = $fail->argv(qw(des -s), "vob:$base")->qx;
    if ($t =~ /lbtype:(.*)$/) {
      $t = $1; $lb = 1;
      die unless $ct->argv(qw(des -s), "lbtype:$t\@$vob")->qx;
    } elsif ($t =~ /brtype:(.*)/) {
      $t = $1; $lb = 0;
      die unless $ct->argv(qw(des -s), "brtype:$t\@$vob")->qx;
    } else {
      if ($ct->argv(qw(des -s), "lbtype:$t\@$vob")->stderr(0)->qx) {
	$lb = 1;
      } elsif ($ct->argv(qw(des -s), "brtype:$t\@$vob")->stderr(0)->qx) {
	$lb = 0;
      } else {
	die Msg('E', 'The argument of the -over flag must be an existing type')
      }
    }
    my $query = $lb? "lbtype($t)" : "version(.../$t/LATEST)";
    $query .= " \&\&! lbtype($lbtype)";
    $base = '-a' if $opt{all};
    @mod = $ct->argv('find', $base, '-ver', $query, '-print')->stderr(0)->qx;
    if ($opt{all} and $ver) {
      use File::Spec::Functions qw(rel2abs);
      $ver = rel2abs($ver);
      @mod = grep /^${ver}(\W|$)/, @mod;
    }
  }
  $mkl->opts(grep !/^-r(ec|$)/, @opt); # recurse handled already
  if ($opt{up}) {
    my $dsc = ClearCase::Argv->new({-autochomp=>1});
    require File::Basename;
    require File::Spec;
    File::Spec->VERSION(0.82);
    my $vroot;
    if ($^O eq 'cygwin') {
      $vroot = '/cygdrive/a';	#just for the length
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
      @elems = () if grep /^-r(ec|$)/, @opt; #already labelled
      push @elems, sort {$b cmp $a} keys %ancestors;
      $ret |= $mkl->args($lbtype, @elems)->system;
      exit $ret;
    }
  }
  if (!$opt{over}) {
    push @elems, @rec;
    @mod = grep {
      my $v = $_;
      $_ = (grep /^$lbtype$/, split/ /,
	    $ct->argv(qw(des -fmt), '%Nl', $v)->qx)? '' : $v;
    } @elems;
  }
  exit $ret unless @mod;
  my $rmattr = ClearCase::Argv->rmattr([("Rm$lbtype")]);
  my @raopts = $rmattr->opts;
  push(@raopts, '-ver', $mkl->flag('version')) if $mkl->flag('version');
  $rmattr->opts(@raopts);
  for (@mod) {
    if (@et) {
      my $rc = $mkl->args($et[0], $_)->system;
      $ret |= $rc;
      next if $rc and !$opt{force};
      $rmattr->args($_)->stderr(0)->system unless $rc;
    }
    @opt = $mkl->opts;
    push @opt, '-rep' if @et and !grep /^-rep/, @opt; #implicit for floating
    $mkl->opts(@opt);
    $ret |= $mkl->args($lbtype, $_)->system;
  }
  exit $ret;
}

=item * CI/CHECKIN

Extended to handle the B<-dir/-rec/-all/-avobs> flags. These are fairly
self-explanatory but for the record B<-dir> checks in all checkouts in
the current directory, B<-rec> does the same but recursively down from
the current directory, B<-all> operates on all checkouts in the current
VOB, and B<-avobs> on all checkouts in any VOB.

Extended to allow B<symbolic links> to be checked in (by operating on
the target of the link instead).

Extended to implement a B<-diff> flag, which runs a B<I<diff -pred>>
command before each checkin so the user can review his/her changes
before typing the comment.

Implements a new B<-revert> flag. This causes identical (unchanged)
elements to be unchecked-out instead of being checked in.

Since checkin is such a common operation, a special feature is supported
to save typing: an unadorned I<ci> cmd is C<promoted> to I<ci -dir -me
-diff -revert>. In other words typing I<ct ci> will step through each
file checked out by you in the current directory and view,
automatically undoing the checkout if no changes have been made and
showing diffs followed by a checkin-comment prompt otherwise.

[ From David Boyce's ClearCase::Wrapper. Adapted to user interactions
preempting. ]

=cut

sub checkin {
  use strict;
  use warnings;
  # Allows 'ct ci' to be shorthand for 'ct ci -me -diff -revert -dir'.
  push(@ARGV, qw(-me -diff -revert -dir)) if grep(!/^-pti/, @ARGV) == 1;
  # -re999 isn't a real flag, it's to disambiguate -rec from -rev. Id. -cr999.
  my %opt;
  GetOptions(\%opt, qw(crnum=s cr999=s diff ok revert re999))
    if grep /^-(crn|dif|ok|rev)/, @ARGV;
  ClearCase::Argv->ipc(1);
  # This is a hidden flag to support DB's checkin_post trigger.
  # It allows the bug number to be supplied as a cmdline option.
  $ENV{CRNUM} = $opt{crnum} if $opt{crnum};
  my $ci = ClearCase::Argv->new(@ARGV);
  # Parse checkin and (potential) diff flags into different optsets.
  $ci->parse(qw(cquery|cqeach nc c|cfile=s
		nwarn|cr|ptime|identical|cact|cwork keep|rm from=s));
  if ($opt{'diff'} || $opt{revert}) {
    $ci->optset('DIFF');
    $ci->parseDIFF(qw(serial_format|diff_format|window columns|options=s
		      graphical|tiny|hstack|vstack|predecessor));
  }
  # Now do auto-aggregation on the remaining args.
  my @elems = AutoCheckedOut($opt{ok}, $ci->args); # may exit
  if (!@elems) {
    warn Msg('E', 'Element pathname required.');
    @ARGV = qw(help checkin);
    ClearCase::Wrapper->help();
    return 1;
  }
  # Turn symbolic links into their targets so CC will "do the right thing".
  for (@elems) {
    $_ = readlink if -l && defined readlink;
  }
  $ci->args(@elems);
  # Give a warning if the file is open for editing by vim.
  # (DB knows, there are lots of other editors but it just happens
  # to be easy to detect vim by its .swp file)
  for (@elems) {
    die Msg('E', "$_: appears to be open in vim!") if -f ".$_.swp";
  }
  if ($opt{diff} or $opt{revert}) {
    # In case ~/.clearcase_profile makes ci -nc the default, make sure
    # we prompt for a comment - unless checking in dirs only.
    if (!grep(/^-c|^-nc$/, $ci->opts) && grep(-f, @elems)) {
      $ci->opts('-cqe', $ci->opts);
      $ci->{AV_LKG}{''}{cquery}=1;
    }
  }
  $ci->{opthashre} = \%opt;
  _Preemptcmt($ci, \&_Checkin, \&_PreCi);
}

=item * RMBRANCH

No semantic change. This implementation is only needed to handle the
optional interactive dialog in the context of the ipc mode of the
underlying I<ClearCase::Argv>.

=cut

sub rmbranch {
  use strict;
  use warnings;
  ClearCase::Argv->ipc(1);
  my $rmbranch = ClearCase::Argv->new(@ARGV);
  $rmbranch->parse(qw(cquery|cqeach nc c|cfile=s force));
  if ($rmbranch->flag('force')) {
    exit $rmbranch->system;
  } else {
    my %forceorabort = (yes => '-force');
    my %yn = (
      format   =>
	q(Remove branch, all its sub-branches and sub-versions? [no] ),
      default  => q(no),
      valid    => qr/yes|no/,
      instruct => "Please answer with one of the following: yes, no\n",
      opt      => \%forceorabort,
    );
    my $des = ClearCase::Argv->des([qw(-s)]);
    $des->stderr(0);
    $des->stdout(0);
    my $exists = sub { return $des->args(shift)->system? 0:1 };
    my $err = q(Pathname not found: );
    my $run = sub { my $rmbranch = shift; $rmbranch->system };
    _Yesno($rmbranch, $run, \%yn, $exists, $err);
  }
}

=item * RMLABEL

For family types, remove both types, and add a I<RmLBTYPE> attribute
mentioning the increment of the removal of the LBTYPE label, for use
in config specs based on sparse fixed types equivalent to a given
state of the floating type.

=cut

sub rmlabel {
  use strict;
  use warnings;
  my $rmlabel = ClearCase::Argv->new(@ARGV);
  $rmlabel->parse(qw(cquery|cqeach nc c|cfile=s recurse follow version=s));
  my($lbtype, @elems) = $rmlabel->args;
  $ct = ClearCase::Argv->new({autochomp=>1});
  if (!($lbtype and @elems)) {
    warn Msg('E', 'Type name required') unless $lbtype;
    warn Msg('E', 'Element pathname required') unless @elems;
    $ct->argv(qw(help rmlabel))->system;
    exit 1;
  }
  if (MSWIN) {
    $_ = glob($_) for @elems;
  }
  $lbtype =~ s/^lbtype://;
  my (%vb, @lt, %et);
  for my $e (@elems) {
    my $v = $ct->argv(qw(des -s), "vob:$e")->stderr(0)->qx;
    $vb{$v}++ if $v;
  }
  if ($lbtype =~ /@(.*)$/) {
    my ($v, @v) = ($1, keys %vb);
    die Msg('E', qq(Object is in unexpected VOB: "$lbtype".))
      if scalar @v > 1 or (scalar @v == 1 and $v !~ /^$v[0]/);
    push @lt, $lbtype;
  } else {
    push @lt, "$lbtype\@$_" for keys %vb;
  }
  $et{$_}++ for grep s/^-> lbtype:(.*)@.*$/$1/,
    map { $ct->argv(qw(des -s -ahl), $eqhl, "lbtype:$_")->qx } @lt;
  my @et = keys %et;
  die Msg('E', qq("$lbtype" must have the same equivalent type in all vobs.))
    if scalar @et > 1;
  my $et = (scalar @et == 1)? $et[0] : '';
  my $fn = sub {
    my ($rml, @cmt) = @_;
    my @opts = $rml->opts;
    my @opcm = @opts;
    push @opcm, @cmt;
    my $rc = 0;
    for (@elems) {
      my $e = $ct->des([qw(-s)], $_)->qx; #in case passed by the label: f@@/L
      $rml->opts(@opcm);
      $rml->args($lbtype, $e);
      my $r1 = $rml->system;
      if ($et) {
	my $att = "Rm$lbtype";
	my $val = $et;
	$val =~ s/^.*_//;
	if (!$r1) { #floating successfully removed
	  my ($f) = ($e =~ /(.*)@@.*$/);
	  my @eqlst = grep s/(.*)/lbtype($1)/, _EqLbTypeList($lbtype);
	  next unless @eqlst;
	  my $query = (@eqlst==1? $eqlst[0] : '(' . join('||', @eqlst) . ')')
	    . "&&!attype($att)";
	  my @v = $ct->find($f, qw(-d -ver), $query, '-print')->qx;
	  $ct->mkattr([$att, $val], @v)->stdout(0)->system if @v;
	}
	$rml->opts(@opts);
	$rml->args($et, $e);
	$r1 |= $rml->stderr(0)->system;
      }
      $rc |= $r1;
    }
    return $rc;
  };
  _Preemptcmt($rmlabel, $fn);
}

=item * RMTYPE

For family label types, 3 cases:

=over

=item -fam: remove all types in the family, as well as the I<RmLBTYPE>
attribute type. This is a rare and destructive situation, unless the
equivalent type is I<LBTYPE_1.00> (the family was just created).
The types actually affected ought of course to be unlocked.

=item -inc: remove the current increment, and move back the family
type onto the previous one. Note: I<RmLBTYPE> attributes ... may be
left behind (for now...)

=item default (no flag): remove the family (floating) type and the
current increment, storing the information about the previous one into
the "hidden" I<LBTYPE_0> type, from which it may be recovered with a
later C<mklbtype -fam LBTYPE>.

=back

Note that removing directly an incremental fixed type is left
unchanged for low level purposes, and thus may corrupt the whole
hierarchy: you need to restore links and take care of possible
I<RmLBTYPE> attributes.

=cut

sub rmtype {
  use strict;
  use warnings;
  my %opt;
  GetOptions(\%opt, qw(f999 family increment)); # f999 to disambiguate -force
  die Msg('E', qq("-family" and "-increment" are mutualy exclusive.))
    if $opt{family} and $opt{increment};
  my $rmtype = ClearCase::Argv->new(@ARGV);
  $rmtype->parse(qw(cquery|cqeach nc c|cfile=s ignore rmall force));
  my @type = $rmtype->args;
  my (@lbt, @oth);
  for (@type) {
    if (/^lbtype:/) {
      push @lbt, $_;
    } else {
      push @oth, $_;
    }
  }
  if (!@lbt) {
    warn Msg('W', '"-family" applies only to label types') if $opt{family};
    warn Msg('W', '"-increment" applies only to label types')
      if $opt{increment};
    exit $rmtype->system;
  }
  my $rs;
  $rs = $rmtype->args(@oth)->system if @oth;
  $ct = ClearCase::Argv->new({autochomp=>1});
  if (!$rmtype->flag('rmall')) {
    my @glb;
    for (@lbt) {
      push @glb, $_
	if $ct->argv(qw(des -fmt %[type_scope]p), $_)->qx eq 'global';
    }
    if (@glb) {
      warn Msg('E', "Global type: must specify removal of all instances.");
      warn Msg('E', qq(Unable to remove label type "$_"))
	for grep { s/^lbtype:(.*)(\@.*)?$/$1/ } @glb;
      exit 1;
    }
  }
  my (@args, @eq, @lck) = @lbt;
  my $lct = ClearCase::Argv->new(); #Not autochomp
  my ($fl, $loaded) = $ENV{FORCELOCK};
 LBT:for my $t (@lbt) {
    my ($eq) = grep s/^-> (lbtype:.*)/$1/,
      $ct->argv(qw(des -s -ahl), $eqhl, $t)->qx;
    if ($eq) {
      my ($base, $vob) = ($1, $2?$2:'') if $t =~ /^lbtype:(.*)(\@.*)?$/;
      if ($opt{family} or $eq =~ /_1.00(\@.*)?$/) {
	push @eq, grep(s/^(.*)/lbtype:${1}$vob/, _EqLbTypeList($t)),
	  "attype:Rm${base}$vob";
      } else {
	my ($prev) = grep s/^-> (lbtype:.*)/$1/,
	  $ct->argv(qw(des -s -ahl), $prhl, $eq)->qx;
	if ($ct->argv(qw(lslock -s), $prev)->stderr(0)->qx) {
	  push @lck, $prev;
	  my @out = $lct->argv('unlock', $prev)->stderr(1)->qx;
	  if (grep /^cleartool: Error/, @out) {
	    if ($fl and !$loaded) {
	      my $fn = $fl; $fn =~ s%::%/%g; $fn .= '.pm';
	      require $fn;
	      $fl->import;
	      $loaded = 1;
	    }
	    if (!$fl) {
	      print @out;
	      next LBT;
	    } else {
	      my ($p, $v) = ($1, $2) if $prev =~ /^lbtype:(.*)\@(.*)$/;
	      next LBT if funlocklt($p, $v);
	    }
	  } else {
	    print @out;
	  }
	}
	if ($opt{increment}) {
	  my ($hl) = grep s/^\s+(.*) -> $eq/$1/,
	    $ct->argv(qw(des -l -ahl), $eqhl, $t)->qx;
	  if ($hl) {
	    $rs |= $ct->argv('rmhlink', $hl)->stdout(0)->system;
	    $rs |= $ct->argv('mkhlink', $eqhl, $t, $prev)->stdout(0)->system;
	    for (@args) {
	      $_ = $eq if $_ eq $t;
	    }
	    # rollback... for all vobs referenced with GlobalDefinition...
	    my @vb = grep s/^\s+.*?\s+.*? -> .*?\@(.*)$/$1/,
	      map{ $ct->argv('des', "hlink:$_")->qx } grep s/^\s+(.+?) <-.*/$1/,
	      $ct->argv(qw(des -l -ahl GlobalDefinition), $eq)->qx;
	    my ($tn, $vb) = ($1, $2) if $eq =~ /^lbtype:(.*)\@(.*)/;
	    push @vb, $vb;
	    my @e = map{$ct->argv('find', $_, qw(-a -ele), "lbtype_sub($tn)",
			      qw(-nxn -print))->qx} @vb;
	    warn Msg('W', qq(Need to move "$base" back on @e.)) if @e;
	  } else {
	    warn Msg('E', qq(Failed to roll "$t" one step back.));
	    $rs = 1;
	  }
	} else {
	  my $t0 = "lbtype:${base}_0$vob"; # store the last eq into hidden type
	  $ct->argv(qw(mklbtype -nc), $t0)->stdout(0)->system;
	  $ct->argv(qw(mkhlink), $eqhl, $t0, $prev)->stdout(0)->system;
	  $ct->argv('lock', $t0)->stdout(0)->system;
	  push @eq, $eq;
	}
      }
    }
  }
  push @args, @eq if @eq;
  $rs |= $rmtype->args(@args)->system;
  for my $l (@lck) {
    my @out = $lct->argv('lock', $l)->stderr(1)->qx;
    if ($fl and grep /^cleartool: Error/, @out) {
      my ($p, $v) = ($1, $2) if $l =~ /^lbtype:(.*)\@(.*)$/;
      flocklt($p, $v); # loaded while unlocking
    } else {
      print @out;
    }
  }
  exit $rs;
}

=item * SETCS

From the version in DSB.pm 1.14--retaining its additions:

Adds a B<-clone> flag which lets you specify another view from which
to copy the config spec.

Adds a B<-sync> flag. This is similar to B<-current> except that it
analyzes the CS dependencies and only flushes the view cache if the
I<compiled_spec> file is out of date with respect to the
I<config_spec> source file or any file it includes. In other words:
B<setcs -sync> is to B<setcs -current> as B<make foo.o> is to
B<cc -c foo.c>.

Adds a B<-needed> flag. This is similar to B<-sync> above but it
doesn't recompile the config spec. Instead, it simply indicates with
its return code whether a recompile is in order.

Adds a B<-expand> flag, which "flattens out" the config spec by
inlining the contents of any include files.

Add support for a incremental label type families, via a
I<##:IncrementalLabels:> attribute in the config spec: generate a
config spec fragment equivalent to the type specified, and include it.
An optional clause of C<-nocheckout> will be propagated to the
generated rules.

=cut

sub setcs {
  use strict;
  use warnings;
  my %opt;
  GetOptions(\%opt, qw(clone=s expand needed sync));
  die Msg('E', "-expand and -sync are mutually exclusive")
    if $opt{expand} && $opt{sync};
  die Msg('E', "-expand and -needed are mutually exclusive")
    if $opt{expand} && $opt{needed};
  my $tag = ViewTag(@ARGV) if grep /^(expand|sync|needed|clone)$/, keys %opt;
  if ($opt{expand}) {
    my $ct = Argv->new([$^X, '-S', $0]);
    my $settmp = ".$::prog.setcs.$$";
    open(EXP, ">$settmp") || die Msg('E', "$settmp: $!");
    print EXP $ct->opts(qw(catcs -expand -tag), $tag)->qx;
    close(EXP);
    $ct->opts('setcs', $settmp)->system;
    unlink $settmp;
    exit $?;
  } elsif ($opt{sync} || $opt{needed}) {
    chomp(my @srcs = qx($^X -S $0 catcs -sources -tag $tag));
    exit 2 if $?;
    (my $obj = $srcs[0]) =~ s/config_spec/.compiled_spec/;
    die Msg('E', "$obj: no such file") if ! -f $obj;
    die Msg('E', "no permission to update $tag's config spec") if ! -w $obj;
    my $otime = (stat $obj)[9];
    my $needed = grep { (stat $_)[9] > $otime } @srcs;
    if ($opt{sync}) {
      if ($needed) {
	ClearCase::Argv->setcs(qw(-current -tag), $tag)->exec;
      } else {
	exit 0;
      }
    } else {
      exit $needed;
    }
  } elsif ($opt{clone}) {
    my $ct = ClearCase::Argv->new;
    my $ctx = $ct->find_cleartool;
    my $cstmp = ".$ARGV[0].$$.cs.$tag";
    Argv->autofail(1);
    Argv->new("$ctx catcs -tag $opt{clone} > $cstmp")->system;
    $ct->setcs('-tag', $tag, $cstmp)->system;
    unlink($cstmp);
    exit 0;
  }
  my $setcs = ClearCase::Argv->new(@ARGV);
  $setcs->parse(qw(force default|current|stream overwrite|rename
		   ctime|ptime tag=s));
  exit $setcs->system if $setcs->flag('force') or $setcs->flag('default')
    or $setcs->flag('overwrite') or $setcs->flag('ctime');
  my ($cs) = $setcs->args;
  if (!$cs) {
    warn Msg('E', 'Configuration spec must be specified.');
    @ARGV = qw(help setcs);
    ClearCase::Wrapper->help();
    return 1;
  }
  my (@cs1, @cs2, $incfam, $noco);
  open my $fh, '<', $cs or die Msg('E', qq(Unable to access "$cs": $!\n));
  while (<$fh>) {
    if (/^\#\#:IncrementalLabels: *([^\s]+)(\s+-nocheckout)?/) {
      ($incfam, $noco) = ($1, $2?$2:'');
      last;
    }
    push @cs1, $_;
  }
  @cs2 = <$fh> if $incfam;
  close $fh;
  exit $setcs->system unless $incfam;
  my ($lbtype, $vob) = $incfam =~ /^(?:lbtype:)?(.*?)\@(.*)$/;
  die Msg('E', qq(Failed to parse the vob from "$incfam"\n)) unless $vob;
  my $rmat = 'Rm' . ($lbtype =~ /^(.*)_/ ? $1 : $lbtype);
  my @eqlst = _EqLbTypeList($lbtype);
  my $nr = $1 if $eqlst[0] =~ /^.*_(\d+\.\d+)$/;
  die Msg('E', qq("$lbtype" is not the top of a label type family\n))
    unless $nr;
  my $ct = ClearCase::Argv->new({autochomp=>1});
  $tag = $ct->argv(qw(pwv -s))->qx unless $tag = $setcs->flag('tag');
  die Msg('E', 'Cannot get view info for current view: not a ClearCase object.')
    unless $tag;
  my ($vws) = reverse split '\s+', $ct->argv('lsview', $tag)->qx;
  open $fh, '>', "$vws/$lbtype"
    or die Msg('E',
	       qq(Failed to write config spec fragment "$vws/$lbtype": $!\n));
  print $fh qq(element * "{lbtype($_)&&!attr_sub($rmat,<=,$nr)}$noco"\n)
    for @eqlst;
  close $fh;
  $cs .= $$;
  open $fh, '>', $cs or die Msg('E', qq(Could not write "$cs": $!\n));
  print $fh @cs1;
  print $fh "include $vws/$lbtype\n";
  print $fh @cs2;
  close $fh;
  my $rc = $setcs->args($cs)->system;
  unlink $cs;
  exit $rc;
}

=back

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2007 IONA Technologies PLC (until v0.05),
2008-2010 Marc Girod (marc.girod@gmail.com) for later versions.
All rights reserved.
This Perl program is free software; you may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1), ClearCase::Wrapper, ClearCase::Wrapper::DSB, ClearCase::Argv

=cut
