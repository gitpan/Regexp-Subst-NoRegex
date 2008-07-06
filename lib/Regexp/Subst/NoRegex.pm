package Regexp::Subst::NoRegex;

use 5.008000;
use strict;
use warnings;

require Exporter;
use AutoLoader qw(AUTOLOAD);

our @ISA = qw(Exporter);

our @EXPORT_OK = qw(rnr_substr rnr_sop);

our $VERSION = '0.01';

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
  } else {
      print "Oh no, more bugs.\n";
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

  $Regexp::Subst::Substr::verbose = 1;

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
    return (\@matches, \%hits);
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
#    print "Values are '$text' '$left' right = '$right'\n";
    my %results;
    # The mother of all evaluations. The regex contains an evaluation,
    # and it itself is evaluated.
    my $toeval = "\$text =~ s/$left/\$results{\$&} = \"$right\";\"$right\"/ge";
    print "Sending '$toeval' to eval\n" if $verbose;
    eval ($toeval);
    print "text is $text, \%results = \n" if $verbose;
    for (keys %results) {
	print "LHS: '$_' RHS: '$results{$_}'\n" if $verbose;
    }
    return \%results;
}

# Internal routine.

# Make a list of [start, end] positions where the non-regex string
# $left is found in $text.

sub make_matches
{
    my ($text, $left) = @_;
    my @matches;
    while ($text =~ m/\Q$left/g) {
	my $p = pos $text;
	print $&, " ", $p - length ($&), " ", $p, "\n" if $verbose;
	push @matches, [$p - length ($&), $p];
    }
    return \@matches;
}

# Internal routine.

# Get a list of whether, for each position that the non-regex LHS of
# the substitution matches, a global substitution of the non-regex LHS
# is correct (matches the regex substitution) or incorrect (a false
# hit which the regex doesn't match). The list is a hash indexed by
# the location of the hit, with the string "good" or "bad" to indicate
# status.

# The second result reports if all the hits are good or not. If all
# the hits are good, then we don't need this list anyway, since a
# blanket substitution will be satisfactory.

sub good_bad
{
    my ($text, $left, $right) = @_;
    my ($left_matches, $hits) = make_regex_matches ($text, $left);
    my %good_bad;
    for (keys %{$hits}) {
	my @good_bad;
	# True if all the hits are good.
	my $all_good = 1;
	print "All matches for '$_': " if $verbose;
	my $all_matches = make_matches ($text, $_);
	for my $match (@{$all_matches}) {
	    my $offset = $$match[0];
	    print "Match '$offset':" if $verbose;
	    if ($$hits{$_}{$offset}) {
		push @good_bad, 'good';
	    } else {
		$all_good = 0;
		push @good_bad, 'bad';
	    }
	    print $good_bad[-1],"\n" if $verbose;
	}
	$good_bad{$_} = $all_good ? 'all' : \@good_bad ;
    }
    return \%good_bad;
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
    my ($text, $left, $right) = @_;
    my $good_bad = good_bad (@_);
    my %markedforsubs;
    my $rhsides = make_dumb_rights($text, $left, $right);
    for my $dumb_left (keys %{$good_bad}) {
	print "\$dumb_left is '$dumb_left'\n" if $verbose;
	# Make the thing to substitute for
	my $dumb_right = $rhsides->{$dumb_left};
	print "\$dumb_right is '$dumb_right'\n" if $verbose;
	my $gb_list = $$good_bad{$dumb_left};
	if (ref ($gb_list) eq 'ARRAY') {
	    print "$dumb_left: ",join (" ",@$gb_list),"\n" if $verbose;
	} else {
	    print "$dumb_left: all good\n" if $verbose;
	}
	my $count = 0;
	while ($text =~ /$dumb_left/g) {
	    print "Match at ",pos($text),": '",
		substr($text,pos($text) - length($dumb_left),10),"'\n" 
		    if $verbose;
	    $count++;
	    print "$count/" if $verbose;
	    if ($gb_list ne 'all') {
		my $gb = shift @{$gb_list};
		if (!$gb) {
		    print "Error: bad number of good/bad items.\n";
		}
		next if ($gb eq 'bad');
	    }
	    print "Marking ",pos($text)," as good.\n" if $verbose;
	    push @{$markedforsubs{$dumb_left}}, [pos($text),$dumb_right];
	}
    }
    my %subs_list;
    for my $dumb_left (keys %{$good_bad}) {
	for (@{$markedforsubs{$dumb_left}}) {
	    my ($end, $dumb_right) = @{$_};
	    my $start = $end - length ($dumb_left);
	    $subs_list{$start} = [$end, $dumb_left, $dumb_right];
	}
    }
    my $offset = 0;
    for my $start ( sort {$a <=> $b} keys %subs_list) {
	my ($end, $dumb_left, $dumb_right) = @{$subs_list{$start}};
	my $l = length ($dumb_left);
	my $r = length ($dumb_right);
	substr ($text, $start + $offset, $l, $dumb_right);
#	print $text;
	$offset += ($r - $l);
    }

    return $text;
}

# Internal routine.

# like make_matches above, but does a substitution into a copy of the
# text, since this is for the s/\Q// (sop) version.

sub make_matches2
{
    my ($text, $left) = @_;
    print "looking for matches for non-regex '$left' in \n$$text\n" if $verbose;
    my @matches;
    my $jibberjabber = join ('', map { '*' } split '', $left);
    print "jibberjabber = '$jibberjabber'\n" if $verbose;
    while ($$text =~ /\Q$left/g) {
	my $p = pos $$text;
	print "'", $&, "' ", $p - length ($&), " ", $p, "\n" if $verbose;
	push @matches, [$p - length ($&), $p];
	$$text =~ s/\Q$left/$jibberjabber/;
    }
    print "text is '$$text'\n" if $verbose;
    return \@matches;
}

# Internal routine.

# like good_bad above, but using make_matches2 to do a substitution
# into a copy of the text in the order of length of the items, since
# this is for the s/\Q// (sop) version.

sub good_bad2
{
    my ($text, $left, $right) = @_;
    my ($left_matches, $hits) = make_regex_matches ($text, $left);
    my %good_bad;
    for (sort { length ($b) <=> length ($a) } keys %{$hits}) {
	my @good_bad;
	# True if all the hits are good.
	my $all_good = 1;
	print "All matches for '$_': \n" if $verbose;
	my $all_matches = make_matches2 (\$text, $_);
	for my $match (@{$all_matches}) {
	    my $offset = $$match[0];
	    print "Match '$offset':" if $verbose;
	    if ($$hits{$_}{$offset}) {
		push @good_bad, 'good';
	    } else {
		$all_good = 0;
		push @good_bad, 'bad';
	    }
	    print $good_bad[-1],"\n" if $verbose;
	}
	$good_bad{$_} = $all_good ? 'all' : \@good_bad ;
    }
    return \%good_bad;
}

=head2 rnr_sop

Regular expression substitution without using any regular expressions
(B<rnr> stands for regex no regex) - emulate

  $text =~ s/$left/$right/g;

with the subsitution operator (s///) with regexes switched off.

  $text =~ s/\Q$x/$y/g;

=over

=item rnr_sop ($L<text>, $L<left>, $L<right>);

=back

=cut

sub rnr_sop
{
    my ($text, $left, $right) = @_;
    my $text2 = $text;
    my $good_bad = good_bad (@_);
    my $rhsides = make_dumb_rights($text, $left, $right);
    print "regex_no_regex: attempting\n$text2\n=~ /$left/$right/g;\n" if $verbose;

    $good_bad = good_bad2 ($text2, $left, $right);
    # Make some identifier for the first substitution pass. I try five
    # different unlikely strings to find something which is definitely
    # not in the original text. Hopefully there are not many documents
    # with all of these things in them.
    my @unlikely = qw/951843 674321 981753 super xyzac/;
    my $separator;
    do {
	$separator = pop @unlikely;
	die "Ran out of separators" if !$separator;
    } while ($text2 =~ /\Q$separator/);
    print "Separator is $separator\n" if $verbose;
    # First pass substitution - globally substitute all the left hand
    # sides with some identifier.

    # Sort the substition keys into length order (longest first) to
    # prevent shorter matches from overriding longer matches. This has
    # to match the behaviour in good_bad2 above.
    my @subkeys = sort { length ($b) <=> length ($a) } keys %{$good_bad};
    # Make an ID.

    # Note on use of sprintf: A weird bug happened when I didn't use
    # sprintf but just tagged the number on, and when the number of
    # substitutions became more than ten, then $separator_11 was being
    # substituted by the right hand side for $separator_1. Aargh!
    # Hence the sprintf.
    for (0..$#subkeys) {
	my $sep = $separator . sprintf ("_%05d", $_);
	$text2 =~ s/\Q$subkeys[$_]/$sep/g;
    }
    print "Text is in substitution limbo as $text2\n" if $verbose;
    # Second pass substitution - substitute the identifiers with the
    # right hand sides. We may have to substitute the identifier with
    # its left hand side if it was a mistake.
    for (0..$#subkeys) {
	my $lhs = $subkeys[$_];
	my $rhs = $rhsides->{$lhs};
	print "trying subs of '$lhs' with '$rhs'\n" if $verbose;
	my $sep = $separator . sprintf ("_%05d", $_);
	my $gb_list = ${$good_bad}{$lhs};
	# Do a global replacement if all of the replacements are OK.
	if (ref ($gb_list) ne 'ARRAY') {
	    print "All good: going global\n" if $verbose;
	    $text2 =~ s/$sep/$rhs/g;
	} else {
	    # Don't do global substitution, do them one at a time.
	    print "Not all good: doing one at a time\n" if $verbose;
	    while ($text2 =~ /$sep/) {
		my $gb = shift @{$gb_list};
		if (!$gb) {
		    die "Internal error: mismatch of good/bad and subs strings.";
		}
		if ($gb eq 'good') {
		    print "Saw a good one\n" if $verbose;
		    $text2 =~ s/$sep/$rhs/;
		} elsif ($gb eq 'bad') {
		    print "Rejected a bad one\n" if $verbose;
		    $text2 =~ s/$sep/$lhs/;
		} else {
		    die "Internal error: invalid value in good/bad array";
		}
	    }
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

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.0 or,
at your option, any later version of Perl 5 you may have available.


=cut

