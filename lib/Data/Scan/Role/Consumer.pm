use strict;
use warnings FATAL => 'all';

package Data::Scan::Role::Consumer;
use Moo::Role;

# ABSTRACT: Data::Scan consumer role

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This the role that every consumer used by L<Data::Scan> must provide. Please refer to L<Data::Scan> for the expected methods signature and return value.

=head1 REQUIRED SUBROUTINES/METHODS

=head2 dsstart

Implementation that will be called when the scanning is starting.

=head2 dsopen

Implementation that will be called when an unfolded content is opened.

=head2 dsread

Implementation that will be called when any item is looked at.

=head2 dsclose

Implementation that will be called when an unfolded content is closed.

=head2 dsend

Implementation that will be called when the scanning is ending.

=cut

requires 'dsstart';
requires 'dsopen';
requires 'dsread';
requires 'dsclose';
requires 'dsend';

=head1 SEE ALSO

L<Data::Scan>

=cut

1;
