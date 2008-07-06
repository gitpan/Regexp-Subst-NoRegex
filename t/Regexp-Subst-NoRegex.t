# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Subst-NoRegex.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 11;
BEGIN { use_ok('Regexp::Subst::NoRegex') };


use Regexp::Subst::NoRegex qw/rnr_substr rnr_sop/;

my $verbose = 0;
my $count = 0;
sub test_dumb_subs
{
    my ($text, $left, $right) = @_;
    # Copy done by Perl using eval.
    my $copy_ok = $text;
    eval ("\$copy_ok =~ s/$left/$right/g");
    # Copies made using dumb substitutions
    my $copy1 = rnr_substr ($text, $left, $right);
    my $copy2 = rnr_sop ($text, $left, $right);
     print "copy1:\n$copy1" if $verbose;
     print "copy2:\n$copy2" if $verbose;
     print "copy_ok:\n$copy_ok" if $verbose;
    $count++;
    print "$count:\n";
    print "1 ", $copy1 eq $copy_ok ? "OK" : "fail", "\n";
    print "2 ", $copy2 eq $copy_ok ? "OK" : "fail", "\n";
    ok ($copy1 eq $copy_ok);
    ok ($copy2 eq $copy_ok);
}
#if (0) {
my $text =<<END;
the quick brown he pe fox jumped over the lazy dog
END
my $left = '\b[bcdfghjklmnpqrstvwxyz]([aeiou])';
my $right = 'X$1Z';
test_dumb_subs($text,$left,$right);
my $text1 =<<END;
don tickle babio monster babio horibble miniature monkey
the quick brown he pe fox jumped over the lazy dog
END
my $left1 = '([aeiou])([bcdfghjklmnpqrstvwxyz])';
my $right1 = 'X$1$2Z';
test_dumb_subs($text1,$left1,$right1);

my $text2 =<<END;
 monster babio horibble miniature monkey
the quick brown he pe fox jumped over the lazy dog
adsadsafo monster babio horibble miniature monkey
the quick brown he pe fox jumped over the lazy dog
END
my $left2 = '\b([bcdfghjklmnpqrstvwxyz]+)([aeiou]?)\b';
my $right2 = 'X$2YZ$1$2Z';
test_dumb_subs($text2,$left2,$right2);

my $text3 =<<END;
colon:   Monkey
test: Monkey
spaces:  Monkey
tricky: Monkey
END
test_dumb_subs($text3,': \s+',': ');

#}
my $text4 =<<END;
please don't break my heart, my achy breaky heart.
it just might not understand. 
Woo wee! Billy Ray Cyrus the virus.

It's just plain old Billy Ray, Cyrus The Virus, trying to sleaze his
way into our hearts again with his treat-your-woman-right,
patronising, slimy puke.

END
my $copy = $text4;
test_dumb_subs($text4,'\b(.)(.*?)\b','$2:$1');

