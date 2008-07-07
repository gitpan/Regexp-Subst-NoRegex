#! perl
use warnings;
use strict;
use File::Temp;

use lib 'C:/Documents and Settings/bkb/My Documents/scripts/modules';

use WordUTF8;

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Regexp-Subst-NoRegex.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use lib './blib/lib';

use Regexp::Subst::NoRegex qw/rnr_substr rnr_sop/;

$Regexp::Subst::NoRegex::verbose = 0;

# Global substitution callback

sub ob
{
#    print "OB @_\n";
}

# One-by-one substitution callback

sub nc
{
    my ($data, $left, $right, $orig, $gb_list) = @_;

    print "NC $data, $left, $right, $orig, (",join (", ",@{$gb_list}),")\n";
}

my %subsop;
$subsop{global} = \&WordUTF8::word_replace;
$subsop{single} = \&WordUTF8::word_replace_selective;
my $document = WordUTF8::get_active_document;
$subsop{data} = $document;

my $verbose =  1;
my $count = 0;
sub test_dumb_subs
{
    my ($text, $left, $right) = @_;
    # Copy done by Perl using eval.
    my $copy_ok = $text;
    eval ("\$copy_ok =~ s/$left/$right/g");
    # Copies made using dumb substitutions
    my $copy1 = rnr_substr ($text, $left, $right);
    my $copy2 = rnr_sop ($text, $left, $right, \%subsop);
     print "copy1:\n$copy1" if $verbose;
     print "copy2:\n$copy2" if $verbose;
     print "copy_ok:\n$copy_ok" if $verbose;
    $count++;
    print "$count:\n";
    print "1 ", $copy1 eq $copy_ok ? "OK" : "fail", "\n";
    print "2 ", $copy2 eq $copy_ok ? "OK" : "fail", "\n";
#    ok ($copy1 eq $copy_ok);
#    ok ($copy2 eq $copy_ok);
}


my $textfile = 'C:/Documents and Settings/bkb/My Documents/scripts/temp/x.txt';

WordUTF8::save_as_text ($document, $textfile);
open my $in, "<:utf8", $textfile or die $!;
my $text;
while (<$in>) { $text .= $_ }
# There is a funky character at the start.
$text =~ s/^.//;
print "The text appears to be '$text'\n";
test_dumb_subs ($text, '(.)(.)', '$2_$1');

