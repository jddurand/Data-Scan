#!perl
use strict;
use warnings FATAL => 'all';
use Test::More tests => 1;
BEGIN {
    use_ok('Data::Scan::Printer') || print "Bail out!\n";
}

use Marpa::R2;
use Data::Printer;

my $this = bless([ 'var1',
                   '2',
                   {'a' => 'b',
                    'c' => [ 'e', 'f' ],
                    'g' => { bless({ 'h' => 'i' }, 'Test::Inner::1') => 'j' },
                    bless({ 'k' => 'l' }, 'Test::Inner::2') => 'm',
                    {} => [],
                    \undef => \\undef,
                   }
                 ], 'Test');
push(@{$this}, { self => $this });
push(@{$this}, { $this => \$this });
push(@{$this}, { $this => $this->[-1] });
p($this);
dspp($this);
done_testing();

1;
