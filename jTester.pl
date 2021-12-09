#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;

$/ = undef;
my $input = <>;

open my $f, '>', '/tmp/program.ijs';
say $f "smoutput $input\nexit ''";
print `jconsole /tmp/program.ijs`;
