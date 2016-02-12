use strict;
use warnings FATAL => 'all';

package Data::Scan::Role::Consumer;
use Moo::Role;

# ABSTRACT: Data::Scan consumer role

# VERSION

# AUTHORITY

requires 'dsstart';
requires 'dsopen';
requires 'dsread';
requires 'dsclose';
requires 'dsend';

1;
