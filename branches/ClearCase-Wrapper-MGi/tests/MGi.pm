package ClearCase::Wrapper::MGi;

$VERSION = '0.09';

use AutoLoader 'AUTOLOAD';
use ClearCase::Wrapper;
use strict;
use vars qw($ct);

#############################################################################
# Usage Message Extensions
#############################################################################
{
  local $^W = 0;
  no strict 'vars';

  my $z = $ARGV[0] || '';
  $lsgenealogy =
    "$z [-short] [-all] [-obsolete] [-depth gen-depth] pname ...";
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
sub sosbranch($$) { # same or sub- branch
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
    return;
  }
  my $ver;
  if ($e =~ m%^(.*?)\@\@.*$%) {
    $ver = $e;
    $e = $1;
    $ver =~ s%[\\/]$%%;
    $ver .= '/LATEST' if $typ eq 'branch';
  }
  if (@$bopt) {
    $$bopt[1] =~ s%[\\/]$%%;
    $ver = $e . "\@\@$$bopt[1]/LATEST";
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

=head1 NAME

ClearCase::Wrapper::MGi - Marc Girod's contributed cleartool wrapper functions

=head1 SYNOPSIS

This is an C<overlay module> for B<ClearCase::Wrapper> containing Marc
Girod's non-standard extensions. See C<perldoc ClearCase::Wrapper> (by
David Boyce) for more details.

=head1 CLEARTOOL EXTENSIONS

=over 5

=item * LSGENEALOGY

New command. B<LsGenealogy> is an alternative way to display the
version tree of an element. It will treat merge arrows on a par level
with parenthood on a branch, and will navigate backwards from the
version currently selected, to find what contributors led to the
version currently selected.
This is thought as being particularly adapted to displaying the
bush-like structure caracteristic of version trees produced under the
advocated branching strategy.

=over 4

=item B<-all> flag

Show 'uninteresting' versions, otherwise skipped:

=over 2

=item - bearing no label

=item - not at a chain boundary.

=back

=item B<-obsolete> flag

Add obsoleted branches to the search.

=item B<-short> flag

Skip displaying labels and 'labelled' versions and do not report
alternative paths or siblings.

=item B<-depth> flag

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

=over 1

=item B<-ver/sion> flag

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
  push @bopts, q(-bra), $opt{branch} if $opt{branch};
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
    $rc |= mkbco($e, undef, \%pbrt, \@bopts, \@gotps, \@copt);
  }
  exit $rc;
}

=item * MKBRANCH

Actually a special case of checkout.

=over 1

=item B<-nco> flag

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
  for (@ARGV[1..$#ARGV]) { $_ = readlink if -l && defined readlink }
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

This wrapper implements a common trigger, to remove the parent branch
if it has no checkouts, no sub-branches, and no remaining versions,
if the version uncheckedout was number 0.

=back

=cut

sub uncheckout {
  my %opt;
  GetOptions(\%opt, qw(ok)) if grep /^-(dif|ok)/, @ARGV;
  for (@ARGV[1..$#ARGV]) { $_ = readlink if -l && defined readlink }
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

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2007 IONA Technologies PLC (until v0.05),
2008-2009 Marc Girod (marc.girod@gmail.com) for later versions.
All rights reserved.
This Perl program is free software; you may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1), ClearCase::Wrapper, ClearCase::Wrapper::DSB

=cut
