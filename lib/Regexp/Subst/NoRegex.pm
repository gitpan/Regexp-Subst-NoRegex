package Regexp::Subst::NoRegex;

use 5.008000;
use strict;
use warnings;

require Exporter;
use AutoLoader qw(AUTOLOAD);

our @ISA = qw(Exporter);

our @EXPORT_OK = qw(rnr_substr rnr_sop);

our $VERSION = '0.02';

=head1 NAME

Regexp::Subst::NoRegex - emulate s/// using s/\Q// or substr

=head1 SYNOPSIS

  use Regexp::Subst::NoRegex qw/rnr_substr rnr_sop/;
  my $text = "ajim jam jom ejeme";
  my $copy = $text;
  $copy =~ s/\bj(.)(.)/$2 $1/g;
  my $copy1 = rnr_substr ($text, '\bj(.)(.)', '$2 $1');
  my $copy2 = rnr_sop ($text, '\bj(.)(.)', '$2 $1');
  if ($copy eq $copy2 && $copy eq $copy1) {
      print "OK\n";
  }

=head1 DESCRIPTION

Given C<$text>, a regex C<$left> and a right hand side C<$right>,
perform the substitution C<$text =~ s/$left/$right/g;> without using
regular expressions for the substitution operation.

The module contains two different algorithms. One, L<rnr_substr>, uses
C<substr> to perform the substitutions, and the other, L<rnr_sop>,
uses multiple non-regex substitutions of the form C<$text =~
/\Q$x/$y/> to emulate Perl's regex substitution while actually
switching it off with the \Q.

=head2 EXPORT

Exports C<rnr_substr> (substr version) and C<rnr_sop> (s/\Q// version)
on request.

=head2 $verbose

Set

  $Regexp::Subst::NoRegex::verbose = 1;

to see what the module is doing.

=cut

our $verbose = 0;

# Internal routine.

# Make a list of [start, end] positions where the regex $left is found
# in $text.

# Bug: this should tell us if the post-substitution stuff is messed up
# by the next substitution of something which comes from the regex. If
# not we don't have to worry about just doing blanket substitutions.

sub make_regex_matches
{
    my ($text, $left) = @_;
    my @matches;
    my %hits;
    while ($text =~ /$left/g) {
	my $p = pos $text;
	my $l = length ($&);
	print "found '",$&, "': ", $p - $l, " ", $p, "\n" if $verbose;
	push @matches, [$&, $p - $l, $p];
	$hits{$&}{$p - $l} = $p;
    }
    my %locs;
    for my $k (keys %hits) {
	my @ls = keys %{$hits{$k}};
	@locs{@ls} = ($k) x @ls;
    }
    # For debugging purposes, make a list ordered by position as well.
    my @loc_list = sort {$a<=>$b} keys %locs;
    for (@loc_list) {
	print "$_: $locs{$_}\n" if $verbose;
    }
    return (\@matches, \%hits, \@loc_list);
}

# Internal routine.

# Given $text, $left and $right, actually do the substitution via
# Perl's s/// operator, and see what right hand sides are obtained.

# Although the routine is short, it contains some truly grotesque code
# involving double evaluations of the substitution.  This one routine
# probably was the most difficult to write of all.

sub make_dumb_rights
{
    my ($text, $left, $right) = @_;
    my %results;
    # The substitution contains an evaluation, and it itself is
    # evaluated.
    my $toeval = "\$text =~ s/$left/\$results{\$&} = \"$right\";\"$right\"/ge";
    print "Sending '$toeval' to eval\n" if $verbose;
    eval ($toeval);
    print "text is $text, \%results = \n" if $verbose;
    for (keys %results) {
	print "LHS: '$_' RHS: '$results{$_}'\n" if $verbose;
    }
    return \%results;
}

=head2 rnr_substr

Regular expression substitution without using any regular expressions
(B<rnr> stands for regex no regex) - emulate

  $text =~ s/$left/$right/g;

with a series of substitutions which use Perl's built in function
C<substr>

  substr ($text, $position, $length) = $y;

=over

=item rnr_substr ($L<text>, $L<left>, $L<right>);

=item text

The text you want to substitute

=item left

Any LHS expression, including regular expressions.

=item right

Any RHS expression, including things like C<$1>, C<$2>, etc.

=back

=cut

sub rnr_substr
{
    $verbose = 0;
    my ($text, $left, $right) = @_;
    my ($matches) = make_regex_matches ($text, $left);
    my %markedforsubs;
    my $rhsides = make_dumb_rights($text, $left, $right);
    my $offset = 0;
    for my $match (@$matches) {
	my ($lhs, $start, $end) = @$match;
	my $rhs = $rhsides->{$lhs};
	my $len = $end - $start;
	print "subsing '$lhs' for '$rhs' at $start, length $len\n" if $verbose;
	substr ($text, $start + $offset, $len) = $rhs;
	$offset += length($rhs) - $len;
	if ($start + $offset < 0 || $offset + $start > length ($text)) {
	    die "Internal error: impossible string offset $offset substituting $lhs for $rhs at position $start of string '$text'";
	}
    }
    $verbose = 0;

    return $text;
}

# Internal routine.

# Find characters which are not being used in the text.

sub find_unused_chars
{
    my ($text) = @_;
    my @unused;
    my %chrsused;
    for (split '', $text) {
	$chrsused{ord($_)}++;
    }
#     for (sort keys %chrsused) {
# 	print "$_: $chrsused{$_}, ";
#     }
#    print "\n";
    # Try ASCII printables first
    for (0x21..0x7E) {
	if (!$chrsused{$_}) {
	    print "found unused character ",chr($_)," (ASCII $_)\n" if $verbose;
	    push @unused, chr($_);
	}
    }
    if (@unused< 2) {
	print "Too many printable ASCII characters in use." if $verbose;
	for (0x80..0xFF) {
	    if (!$chrsused{$_}) {
		print "found unused character ",chr($_),"(non-ASCII $_)\n" if $verbose;
		push @unused, chr($_);
	    }
	}
    }
    if (@unused< 2) {
	die "Too many characters in use in this text. This routine needs at least two characters to not be used.";
    }
    return @unused;
}

# Internal routine.

sub make_matches2
{
    $verbose = 0;
    my ($text, $left, $unusedchar) = @_;
    print "looking for matches for non-regex '$left' in \n$$text\n" if $verbose;
    print "012345678901234567890123456789012345678901234567890\n" if $verbose;
    my @matches;
    my $p = 0;
    while ($$text =~ /\Q$left/g) {
 	my $p = pos $$text;
 	print "'", $&, "' ", $p - length ($&), " ", $p, "\n" if $verbose;
 	push @matches, $p - length ($&);#, $p];
    }
    $verbose = 0;
    return \@matches;
}

# Internal routine.

# like good_bad above, but using make_matches2 to do a substitution
# into a copy of the text in the order of length of the items, since
# this is for the s/\Q// (sop) version.

sub good_bad
{
    my ($text, $left, $right, $unusedchar) = @_;
    my ($left_matches, $hits) = make_regex_matches ($text, $left, $unusedchar);
    my %good_bad;
    my @keyz = sort { length ($b) <=> length ($a) } keys %{$hits};
    for (@keyz) {
	my @good_bad;
	# True if all the hits are good.
	my $all_good = 1;
	print "All matches for '$_': \n" if $verbose;
	my $all_matches = make_matches2 (\$text, $_, $unusedchar);
	print "There are ",scalar(@{$all_matches})," all_matches\n" if $verbose;
	for my $match (@{$all_matches}) {
	    my $offset = $match;
	    print "Match '$offset':" if $verbose;
	    if ($$hits{$_}{$offset}) {
		print "Hit for $_ at $offset\n" if $verbose;
		push @good_bad, 'good';
	    } else {
		print "Miss for $_ at $offset\n" if $verbose;
		$all_good = 0;
		push @good_bad, 'bad';
	    }
	    print $good_bad[-1],"\n" if $verbose;
	}
	$good_bad{$_} = $all_good ? 'all' : \@good_bad ;
	# Substitute out the good matches
	my $jibberjabber = $unusedchar x length ($_);
	print "jibberjabber = '$jibberjabber'\n" if $verbose;
	if ($all_good) {
	    $text =~ s/\Q$_/$jibberjabber/g;
	} else {
	    my $p = 0;
	    for my $i (0..$#good_bad) {
		$p = @{$all_matches}[$i];
		if ($good_bad[$i] eq 'good') {
		    substr ($text, $p, length($_), $jibberjabber);
		} elsif ($good_bad[$i] eq 'bad') {
		    # skip
		} else {
		    print "Problem in size of \@good_bad\n";
		}
		$p++;
	    }
	}
	print "\$text is now\n$text\n" if $verbose;
    }
    return (\%good_bad, \@keyz);
}

# Substitute s/// away the identifier $left either into $right, if it
# is a good match, or $orig, the string which the identifier replaced,
# if it is a bad match.

sub sop_good_bad
{
    $verbose = 0;
    my ($text, $left, $right, $sep, $gb_list) = @_;
    my @gbs = @{$gb_list};
    print "There are ",scalar(@gbs)," good/bad matches\n" if $verbose;
    print "Not all good: doing one at a time\n" if $verbose;
    my $nsubs = $$text =~ s/\Q$left/$sep/g;
    if ($nsubs != @gbs) {
	die "Internal error: mismatch of good / bad list & number of substitutions.";
    }
    print "Made $nsubs substitutions\n" if $verbose;
    while ($$text =~ /\Q$sep/) {
#	print "$`\[$&\]$'\n";
	my $gb = shift @gbs;
	if (!$gb) {
	    die "Internal error: mismatch of good/bad and subs strings while looking for '$sep'.";
	}
	if ($gb eq 'good') {
	    print "Saw a good one\n" if $verbose;
	    $$text =~ s/\Q$sep/$right/ or 
		die "Internal error: single substitution of '$left' with '$right' failed.\n";
	} elsif ($gb eq 'bad') {
	    print "Rejected a bad one\n" if $verbose;
	    $$text =~ s/\Q$sep/$left/ or 
		die "Internal error: single substitution of '$left' with '$right' failed.\n";
	} else {
	    die "Internal error: invalid value in good/bad array";
	}
    }
    $verbose = 0;
}

# Make a unique separator which only has characters which don't get
# re-substituted - a big problem in previous versions of the code.

# At the moment, this makes a separator either of the form x23x, where
# 23 is the number, or of the form xyyyx, where x and y are two
# characters unused in $text, as computed by find_unused_chars
# above. The second option is for the case where there is a
# substitution consisting of digits only.

sub make_sep 
{
    my ($unusedchar, $n, $usedigits) = @_;
    my $sep;
    if ($usedigits) {
	$sep = $unusedchar->[0].$n.$unusedchar->[0];
    } else {
	if (!$unusedchar->[1]) {
	    die "Don't have enough unused characters.";
	}
	$sep = $unusedchar->[0].$unusedchar->[1] x ($n+1).$unusedchar->[0];
    }
    return $sep;
}

=head2 rnr_sop

Regular expression substitution without using any regular expressions
(B<rnr> stands for regex no regex) - emulate

  $text =~ s/$left/$right/g;

with the subsitution operator (s///) with regexes switched off.

  $text =~ s/\Q$x/$y/g;

=over

=item rnr_sop ($L<text>, $L<left>, $L<right>, $L<subsop>);

=item subsop

This is a hash reference containing

=over 2

=item $subsop{global}

callback routine for global substitutions

Takes arguments ($data, $left, $right) where $left should be globally
substituted for $right, and $data is the data from $subsop{data}.

=item $subsop{single}

callback routine for sequential substitutions

Takes arguments ($data, $left, $right, $orig, $gb_list);

where $gb_list is a list of good or bad substitutions in the form
('good', 'bad', 'bad', 'good', 'good'), and $left should be
substituted with either $right or $orig depending on whether $gb_list
says "good" or "bad". $data is $subsop{data}.

=back

=item $subsop{data}

data to send to the callback routines.

=back

=cut

sub rnr_sop
{
    my ($text, $left, $right, $subsop) = @_;
    my $text2 = $text;
    my $rhsides = make_dumb_rights($text, $left, $right);
    print "rnr_sop: attempting\n$text2\n=~ /$left/$right/g;\n" if $verbose;
    my @unusedchars = find_unused_chars ($text);
    my ($good_bad, $keyz) = good_bad ($text2, $left, $right, $unusedchars[0]);
    # First pass substitution - globally substitute all the left hand
    # sides with some identifier.

    # Make sure we use the same list of keys as in good_bad, or
    # mystery bugs will happen.
    my @subkeys = @{$keyz};
    my $usedigits = 1;
    for (@subkeys) { $usedigits = 0 if /^\d+$/ }
    print "Text is in substitution limbo as $text2\n" if $verbose;
    # First pass substitution - substitute valid identifiers with the
    # right hand sides.
    for (0..$#subkeys) {
	my $lhs = $subkeys[$_];
	my $sep1 = make_sep (\@unusedchars, (2*($_+1)), $usedigits);
	my $sep2 = make_sep (\@unusedchars, (2*($_+1)+1), $usedigits);
	my $gb_list = ${$good_bad}{$lhs};
	# Do a global replacement if all of the replacements are OK.
	if (ref ($gb_list) ne 'ARRAY') {
	    if ($text2 !~ s/\Q$lhs/$sep2/g) {
		die "Substitution failed";
	    }
	    print "s/\\Q$lhs/$sep2/g gives\n" if $verbose;
	    print "$text2\n" if $verbose;
	    if ($subsop && $subsop->{global}) {
		&{$subsop->{global}}($subsop->{data}, $lhs, $sep2)
	    }
	} else {
	    # Don't do global substitution, do them one at a time.
	    sop_good_bad (\$text2, $lhs, $sep2, $sep1, $gb_list);
	    if ($subsop && $subsop->{single}) {
		&{$subsop->{single}}($subsop->{data}, $lhs, $sep2, $sep1, $gb_list);
	    }
	}
    }
    # Second pass substitution. Substitute the keys with identifiers.
    for (0..$#subkeys) {
	my $sep2 = make_sep (\@unusedchars, (2*($_+1)+1), $usedigits);
	my $lhs = $subkeys[$_];
	my $rhs = $rhsides->{$lhs};
	$text2 =~ s/$sep2/$rhs/g;
	if ($subsop && $subsop->{global}) {
	    &{$subsop->{global}}($subsop->{data}, $sep2, $rhs)
	}
    }
    return ($text2);
}

1;

__END__

# =head2 Algorithm

# The algorithm to do this substitution is complicated. Consider the
# following:

#   my $string = "ba ba black sheep abba bca";
#   $string = s/\bb(.*?)a/B$1A/g;

# After the substitution, $string contains

#   BA BA BlAck sheep abba BcA

# Note that the substitition did not replace the ba in abba, because of
# the \b, which forces a match at a word boundary. To make the
# substitution without regex, the first obvious idea is to get a list of
# all matches and then substitute each one:

#   my $string = "ba ba black sheep abba bca";
#   my $regex = '\bb(.*?)a';
#   my $right = 'B$1A';
#   my @matches;
#   while ($string =~ /$regex/g ) { push @matches, $& }
#   my @rhs = @matches;
#   @rhs = map { eval "s/$regex/$right/"; $_ } @rhs;

# This gives us two lists

#   @matches = ba ba bla bca
#   @rhs     = BA BA BlA BcA

# Then if we try to do

#   for (0..$#matches) {
#       eval ("\$final2 =~ s/\Q$matches[$_]\E/$rhs[$_]/g");
#   }

# we get

#   BA BA BlAck sheep abBA BcA

# But abba is now abBA, which we didn't want - we had a \b in the regex
# version. In this module, this excess substitution problem is solved by
# making a list of good and bad substitutions and then going through the
# list and matching it.

# Further complexity arises from problems with things where the regex
# could match the same string in more than one way. The substr
# version,L<rnr_substr>, solves this problem by going through the text
# in order of position of the match. The sop version, L<rnr_sop>, solves
# this by going through the text in order of longest match first.

# =head3 Terminology

# =over

# =item LHS

# The left hand side of the substitution, LHS in

#   s/LHS/RHS/;

# =item regex LHS

# The regex left hand side of the substitution. Suppose we have a string

#   my $string = "ba ba black sheep abba bca";

# and we want to match it with a regex like

#   my $regex = '\bb.*?a'

# then regex LHS means '\b.*?a'.

# =item non-regex LHS

# The non-regex left hand side of the substitution, the values that the
# left hand side takes after all the regex bits are expanded. In the
# example above, the non-regex LHSs would be

#   my @matches = ('ba', 'ba', 'bla', 'bca');

# =item RHS

# The right hand sides of the substitution, RHS in an expression like

#   s/LHS/RHS/g;

# =item sop

# The Perl documentation doesn't give s/// a name except s///, which
# isn't useable as part of the routine name, so I've called it "sop" for
# "s operator" or "substitution operator".

# =back

=head1 AUTHOR

Ben Bullock, E<lt>benkasminbullock@gmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008 by Ben Kasmin Bullock. 

This module is distributed under the same terms as Perl itself, either
Perl version 5.10.0 or, at your option, any later version of Perl 5
you may have available.

=cut

