use strict;
use warnings FATAL => 'all';

package Data::Scan::Role::Consumer;
use Moo::Role;

# ABSTRACT: Data::Scan consumer role

# VERSION

# AUTHORITY

requires 'start';
requires 'sopen';
requires 'sread';
requires 'sclose';
requires 'end';

1;
