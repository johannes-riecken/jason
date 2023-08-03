#!/usr/bin/perl -w
package Formatter;
use v5.30;
use Data::Dumper;
use List::Util qw(max);

sub main {
    $/ = undef;
    $_ = <STDIN>;
    say jToLol($_);
}

sub jToLol {
    my ($in) = @_;
    $Data::Dumper::Indent = 0;
    $Data::Dumper::Terse = 1;
    my $max = max(map { length } ($in =~ /(\n+)/g));
    return Data::Dumper::Dumper([splitAtLevel($max, $in)]);
}

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

main unless caller;
