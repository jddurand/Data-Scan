use strict;
use warnings FATAL => 'all';

package Data::Scan::Role::Option;
use Moo::Role;

# ABSTRACT: Data::Scan option role

# VERSION

# AUTHORITY

requires 'wanted';
requires 'unfold';
requires 'process';

1;
