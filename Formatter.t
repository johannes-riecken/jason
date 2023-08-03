#!/usr/bin/perl -w
package Formatter;
use v5.30;
use Data::Dumper;
use List::Util qw(max);
use FindBin qw($Bin);
use lib "$Bin";
use Formatter;
use Test::More;

is_deeply jToLol("1\n"), "1\n", '0-dimensional (scalar)';
# is_deeply jToLol("1 2 3\n"), "[[1,2,3]]\n", '1-dimensional (vector)';
# is_deeply jToLol("1 2 3\n4 5 6"), "[[1,2,3],[4,5,6]]", '2-dimensional (matrix)';
# is_deeply jToLol("1 2 3\n4 5 6\n\n7 8 9\n10 11 12\n"),
#     "[[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]]",
#     "3-dimensional";

done_testing();

