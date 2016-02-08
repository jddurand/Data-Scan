use strict;
use warnings FATAL => 'all';

package Data::Scan::Role::Consumer;
use Moo::Role;

# ABSTRACT: Data::Scan consumer role

# VERSION

# AUTHORITY

requires 'start';
requires 'process';
requires 'endfold';
requires 'end';
requires 'output';

1;
