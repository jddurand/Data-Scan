use strict;
use warnings FATAL => 'all';

package Data::Scan::Role::Consumer;
use Moo::Role;

# ABSTRACT: Data::Scan consumer role

# VERSION

# AUTHORITY

requires 'start';
requires 'process';
requires 'push';
requires 'pop';
requires 'end';

1;
