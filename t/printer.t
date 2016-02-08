#!perl
use strict;
use warnings FATAL => 'all';
use Test::More;
BEGIN {
    use_ok('Data::Scan::Printer') || print "Bail out!\n";
}

use Marpa::R2;
use Data::Printer;

my $this = bless([ 'Level1[0]', 'Level1[1]', undef, \undef, \\undef ], 'TEST');
dspp($this);
done_testing();
exit;

p($this);

$this = Marpa::R2::Scanless::G->new({source => \'x ::= \'x\''});
dspp($this);
done_testing();

1;
