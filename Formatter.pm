#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;
use List::Util qw(max);

$/ = undef;
$Data::Dumper::Indent = 0;
$Data::Dumper::Terse = 1;
$_ = <STDIN>;

my $max = max(map { length } /(\n+)/g);

# splitAtLevel splits one axis
sub splitAtLevel {
    my ($level, $text) = @_;
    if ($level == 0) {
        return map { 0 + $_ } split(delim($level), $text);
    }
    my @a = split(delim($level), $text);
    return map { [splitAtLevel($level - 1, $_)] } split(delim($level), $text);
}

# delim returns the delimiter used at the level n
sub delim {
    my ($n) = @_;
    if ($n == 0) {
        return ' ';
    }
    return "\n" x $n;
}

say Dumper [splitAtLevel($max, $_)];
