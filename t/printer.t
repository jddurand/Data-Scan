#!perl
use strict;
use warnings FATAL => 'all';
use Test::More;
BEGIN {
    use_ok('Data::Scan::Printer') || print "Bail out!\n";
}

dspp(bless [ 'Level1[0]', 'Level1[1]', undef, \undef, \\undef ], 'TEST');
use Marpa::R2;
dspp(Marpa::R2::Scanless::G->new({source => \'x ::= \'x\''}));
use Data::Printer;
p(Marpa::R2::Scanless::G->new({source => \'x ::= \'x\''}));
done_testing();

1;
