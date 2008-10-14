package ClearCase::Wrapper::MGi;

$VERSION = '0.07';

use AutoLoader 'AUTOLOAD';
use ClearCase::Wrapper;
use strict;

#############################################################################
# Usage Message Extensions
#############################################################################
{
   local $^W = 0;
   no strict 'vars';

   $checkout = '';
   my $z = $ARGV[0] || '';
   $lsgenealogy =
       "$z [-short] [-all] [-obsolete] [-depth gen-depth] pname ...";
}

#############################################################################
# Command Aliases
#############################################################################
*co             = *checkout;
*lsgen		= *lsgenealogy;

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
	for my $c (@s) { $cprinted++ if $$gen{$c}{printed}; }
	if ($cprinted) {
	    if ($ind == 0) {
		print "\[offsprings:";
	    } else {
		my $pind = $ind - 1;
		printf("%${pind}s\[siblings:", '');
	    }
	    foreach (@u) { print " $_"; }
	    print "\]\n";
	}
    }
    if ($opt{all}
	or $l
	or (scalar(@p) != 1)
	or (scalar(@s) != 1)
	or !sosbranch($id, $s[0])
	or !sosbranch($p[0], $id))
    {
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
	if ($$gen{$id}{depth} <= $$gen{$p}{depth}) {
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
    my ($ct, $v) = @_;
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
    my ($pbrt, $ct, $bt) = @_;
    if (!defined($pbrt->{$bt})) {
	my $tc = $ct->argv('des', qw(-fmt %[type_constraint]p),
			   "brtype:$bt")->qx;
	$pbrt->{$bt} = ($tc =~ /one version per branch/);
    }
    return $pbrt->{$bt};
}

=head1 NAME

ClearCase::Wrapper::MGi - Marc Girod's contributed cleartool wrapper functions

=head1 SYNOPSIS

This is an C<overlay module> for B<ClearCase::Wrapper> containing Marc
Girod's non-standard extensions. See C<perldoc ClearCase::Wrapper> (by
David Boyce) for more details.

=head1 CLEARTOOL EXTENSIONS

=over 2

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

=item 1. B<-all> flag

Show 'uninteresting' versions, otherwise skipped:
  - bearing no label
  - not at a chain boundary.

=item 2. B<-obsolete> flag

Add obsoleted branches to the search.

=item 3. B<-short> flag

Skip displaying labels and 'labelled' versions and do not report
alternative paths or siblings.

=item 4. B<-depth> flag

Specify a maximum depth at which to stop displaying the genealogy of
the element.

=back

=cut

sub lsgenealogy {
    GetOptions(\%opt, qw(short all obsolete depth=i));
    Assert(@ARGV > 1);	# die with usage msg if untrue
    shift @ARGV;
    my @argv = ();
    for (@ARGV) {
	$_ = readlink if -l && defined readlink;
	push @argv, MSWIN ? glob($_) : $_;
    }
    my $ct = ClearCase::Argv->new({autofail=>0,autochomp=>1,stderr=>0});
    $ct->ipc(1) unless $ct->ctcmd(1);

    while (my $e = shift @argv) {
	my ($ele, $ver, $type, $pred) =
	  $ct->argv(qw(des -fmt), '%En\n%En@@%Vn\n%m\n%En@@%PVn', $e)->qx;
	if (!defined($type) or ($type !~ /version$/)) {
	    warn Msg('W', "Not a version: $e");
	    next;
	}
	$ele =~ s%\\%/%g;
	$ver =~ s%\\%/%g;
	$pred =~ s%\\%/%g;
	my $obsopt = $opt{obsolete}?' -obs':'';
	my @vt = grep m%/([1-9]\d*|CHECKEDOUT)( .*)?$%,
	  $ct->argv('lsvtree', '-merge', "-all$obsopt", $ele)->qx;
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
	setdepths($ver, 0, \%gen);
	my %seen = ();
	printparents($ver, \%gen, \%seen, 0);
    }
    exit 0;
}

=item * CO/CHECKOUT

Supports the BranchOff feature, which you can set up via an attribute
in the config spec.  The rationale and the design are documented in:

 http://www.cmcrossroads.com/cgi-bin/cmwiki/view/CM/BranchOffMain0

Instead of branching off the selected version, the strategy is to
branch off the root of the version tree, copy-merging there from the
former.

This allows to avoid both merging back to /main or to a delivery
branch, and to cascade branches indefinitely.  The logical version tree
is restituted by navigating the merge arrows, to find all the direct or
indirect contributors.

=back

=cut

sub checkout {
    for (@ARGV[1..$#ARGV]) { $_ = readlink if -l && defined readlink }
    my $ct = ClearCase::Argv->new({autochomp=>1});
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
    return 0 if grep /^-(bra|ver)/, @ARGV[1..$#ARGV];
    my %opt;
    GetOptions(\%opt, qw(reserved unreserved nmaster out=s ndata ptime
			 nwarn c=s cfile=s cq cqe nc
			 query nquery usehijack));
    if (my @o = grep /^-/, @ARGV) {
	if (!(grep /^--$/, @o)) {
	    warn Msg('E', "Unsupported options: @o");
	    exit(1);
	}
    }
    my @gopts = ();
    push @opts, q(-res) if $opt{reserved};
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
	for (@ARGV[1..$#ARGV]) { push @args, glob($_); }
    } else {
	@args = @ARGV[1..$#ARGV];
    }
    my $rc = 0;
    my %pbrt = ();
    foreach my $e (@args) {
	if ($ct->argv(qw(des -fmt %m), $e)->qx !~ /version$/) {
	    warn Msg('W', "Not a version: $e");
	    next;
	}
	my ($ver, $bt) = ();
	my $sel = $ct->argv('ls', '-d', "$e")->qx;
	if ($sel =~ /^(.*?) +Rule:.*-mkbranch (.*?)\]?$/) {
	    ($ver, $bt) = ($1, $2);
	}
	if (checkcs($ct, $e) and $bt) {
	    my $main = ($ct->argv('lsvtree', $e)->qx)[0];
	    $main =~ s%^[^@]*\@\@[\\/](.*)$%$1%;
	    my $vob = $ct->argv('des', '-s', "vob:$e")->qx;
	    my $re = pbrtype(\%pbrt, $ct, "$bt\@$vob") ?
		qr([\\/]${main}[\\/]$bt[\\/]\d+$) : qr([\\/]$bt[\\/]\d+$);
	    if ($ver =~ m%$re%) {
		$ct->argv('co', @gopts, @copt, $e);
		$rc |= $ct->system;
	    } else {
		my $r = $ct->argv('mkbranch', @copt,
				  '-ver', "/${main}/0", $bt, $e)->system;
		if ($r) {
		    $rc = 1;
		} else {
		    if ($ver !~ m%\@\@[\\/]${main}[\\/]0$%) {
		        $rc |= $ct->argv('merge', '-to', $e, $ver)->system;
		    }
		}
	    }
	} else {
	    $rc |= $ct->argv('co', @gopts, @copt, $e)->system;
	}
    }
    exit $rc;
}

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2007 IONA Technologies PLC (mgirod@iona.com). All rights
reserved.  This Perl program is free software; you may redistribute it
and/or modify it under the same terms as Perl itself.

=head1 SEE ALSO

perl(1), ClearCase::Wrapper, ClearCase::Wrapper::DSB

=cut
