package ClearCase::Wrapper::MGi;

$VERSION = '0.19';

use warnings;
use strict;
use vars qw($ct $eqhl $prhl $diat);
($eqhl, $prhl, $diat) = qw(EqInc PrevInc DelInc);

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
  $lsgenealogy =
    "$z [-short] [-all] [-obsolete] [-depth gen-depth] pname ...";
  $mklbtype = "\n* [-family] [-increment] [-archive] pname ...";
  $mkbrtype = "\n* [-archive]";
  $mklabel = "\n* [-up] [-force] [-over type [-all]]";
  $checkin = "\n* [-dir|-rec|-all|-avobs] [-ok] [-diff [diff-opts]] [-revert]";
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
      push @args, @opts, @cmt, $e; # Ensure non empty array
      $rc |= $ct->argv('co', @args)->system;
    }
  }
  return $rc;
}
sub _Ensuretypes(@) {
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
sub _Pfxmajminsfx($) {
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
      $ret = &$fn($cmd, qw(-nc));
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
      $ret = &$fn($cmd, @copt);
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
	next if $tst and !&$tst($cmd, $arg);
	$cmd->args($arg);
	print qq(Comments for "$arg":\n);
      } else {
	$go = 0;
	last if $tst and !&$tst($cmd, @arg); #None checked out
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
	$ret |= &$fn($cmd, '-cfile', $cfile);
      } else {
	$ret |= &$fn($cmd, '-c', $cmt);
      }
    }
  }
  exit $ret;
}
sub _Unco {
  use warnings;
  use strict;
  my $unco = shift;
  $ct = ClearCase::Argv->new({autochomp=>1});
  my @b0 = grep { m%[\\/]CHECKEDOUT$% }
    $ct->argv(qw(ls -s -d), $unco->args)->qx;
  my $rc = $unco->system;
  map { s%^(.*)[\\/]CHECKEDOUT$%$1% } @b0;
  my @rm = ();
  for my $d (@b0) {
    opendir BR, $d or next;
    my @f = grep !/^\.\.?$/, readdir BR;
    closedir BR;
    push @rm, $d if (scalar @f == 2) and $f[0] eq '0' and $f[1] eq 'LATEST';
  }
  $ct->argv(qw(rmbranch -f), @rm)->system if @rm;
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
      my $res = $test->args($arg)->qx;
      if (!$res or $res =~ /^cleartool: Error:/) {
	warn ($res or Msg('E', $errmsg . '"' . $arg . "\".\n"));
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
      $ret |= &{$fn}($cmd);
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
    my $err = 'Unable to find checked out version for ';
    my $run = sub { my $ci = shift; $ci->system };
    _Yesno($ci, $run, \%yn, $lsco, $err); #only one arg: may exit
  } else {
    return $ci->system;
  }
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

=over 12

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
    die if $ct->argv(qw(des -s), "brtype:$bt$_")->stdout(0)->system;
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
    my $err = 'Unable to find checked out version for ';
    _Yesno($unco, \&_Unco, \%yn, $lsco, $err);
  }
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

Equivalent incremental fixed type. A I<fixed> (i.e. which by
convention, will not been I<moved>), I<sparse> (i.e. applied only to
changes) type. This helps to mark the history of application of the
I<floating> type, which is also a I<full> one, for reproducibility
purposes.

=item B<PrevInc>

Previous incremental fixed type.

=back

One attribute:

=over 1

=item B<DelInc>

Deleted from increment. This functionality is not implemented yet.

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

=item B<-arc/hive>

Rename the current type to an I<archive> value (name as prefix, and a
numeral suffix. Initial value: I<-001>), create a new type, and make the
archived one its predecessor, with a B<PrevInc> hyperlink.
Comments go to the type being archived.

The implementation is largely shared with I<mkbrtype>.

=item B<-glo/bal>

Global types (in an Admin vob or not) are incompatible with the family
property.

=back

=cut

sub _GenMkTypeSub {
  use strict;
  use warnings;
  my ($type, $Name, $name) = @_;
  return sub {
    my ($ntype, @cmt) = @_;
    my $rep = $ntype->{rep};
    $ct = ClearCase::Argv->new({autochomp=>1});
    my @args = $ntype->args;
    my %opt = %{$ntype->{fopts}};
    my $silent = $ct->clone;
    $silent->stdout(0);
    if ($ntype->flag('global')) {
      $ntype->opts(@cmt, $ntype->opts);
      return $ntype->system
    }
    if (!%opt and my ($ahl) = grep /^->/,
	$ct->desc([qw(-s -ahl AdminVOB vob:.)])->qx) {
      if (my $avob = (split /\s+/, $ahl)[1]) {
	for (@args) {
	  next if /\@/;
	  $_ = "$_\@$avob";
	  warn Msg('W', "making global type $_ ...");
	}
	$ntype->args(@args);
	my @opts = (@cmt, '-global', $ntype->opts);
	push @opts, '-replace' if $rep;
	$ntype->opts(@opts);
	return $ntype->system;
      }
    } elsif (%opt) {
      map { s/^${type}:(.*)$/$1/ } @args;
      my (@a, $unkvob) = @args;
      my @vob = grep { s/[^@]+\@(.*)$/$1/ } @a;
      for (@vob) {
	$unkvob++ if $silent->argv(qw(des -s), "vob:$_")->system;
      }
      return 1 if $unkvob;
      push @vob, $ct->argv(qw(des -s vob:.))->stderr(0)->qx
	if grep !/@/, @args;
      die  Msg('E', qq(Unable to determine VOB for pathname ".".))
	unless @vob;
      _Ensuretypes(@vob);
      if ($rep) {
	@args = grep { $ct->argv(qw(des -fmt), '%Xn', "$type:$_")->qx } @args;
	exit 1 unless @args;
	if ($opt{family}) {
	  @a = ();
	  foreach my $t (@args) {
	    if ($ct->argv(qw(des -s -ahl), $eqhl, $t)->stderr(0)->qx) {
	      warn Msg('E', "$t is already a family type\n");
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
	  my %pair = ();
	  foreach (@args) {
	    if (/^(.*?)(@.*)?$/) {
	      $pair{$_} = "${1}_1.00" . ($2? $2:'');
	    }
	  }
	  _Findfreeinc(\%pair);
	  my @opts = $ntype->opts();
	  $ntype->args(values %pair);
	  $ntype->opts(@cmt, @opts);
	  $ntype->system;
	  $ntype->args(@args);
	  $ntype->opts('-nc', @opts);
	  $ntype->system;
	  map {
	    if (defined($pair{$_})) {
	      my $inc = "$type:$pair{$_}";
	      $silent->argv('mkhlink', $eqhl, "$type:$_", $inc)->system;
	    }
	  } keys %pair;
	} elsif ($opt{increment}) { # increment
	  $ntype->opts(@cmt, $ntype->opts);
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
	    my $lct = ClearCase::Argv->new(); #Not autochomp
	    my ($fl, $loaded) = $ENV{FORCELOCK};
	    my ($t1) = $t =~ /^(.*?)(@|$)/;
	    for my $l ($t1, $prev) {
	      if ($ct->argv(qw(lslock -s), "lbtype:$l\@$vob")->stderr(0)->qx) {
		$lck = 1; #remember to lock the equivalent fixed type
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
		  } elsif (flocklt($prev, $vob)) {
		    next INCT;
		  }
		} else {
		  print @out;
		}
	      }
	    } else {
	      warn "Previous increment non suitable in $t: $prev\n";
	      next;
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
  die Msg('E', 'Incompatible options: incremental types cannot be global')
    if %opt and grep /^-glo/, @ARGV;
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

=over 2

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
	     (!$currlock or $opt{iflocked}) or $lock->flag('replace')) {
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
	} elsif	(funlocklt($lt, $v)) {
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
  $mkl->parse(qw(replace|recurse|ci|cq|nc
		 version|c|cfile|select|type|name|config=s));
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
  $ret = $mkl->args($et[0], @mod)->system if @et;
  exit $ret if $ret and !$opt{force};
  @opt = $mkl->opts;
  push @opt, '-rep' if @et and !grep /^-rep/, @opt; #implicit for floating
  $mkl->opts(@opt);
  $ret |= $mkl->args($lbtype, @mod)->system;
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
underlying I<ClearCase::Argv>, to work around a bug in I<cleartool status>.

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
    my $exists = ClearCase::Argv->des([qw(-s)]);
    $exists->stderr(1);
    my $err = q(Pathname not found: );
    my $run = sub { my $rmbranch = shift; $rmbranch->system };
    _Yesno($rmbranch, $run, \%yn, $exists, $err);
  }
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
