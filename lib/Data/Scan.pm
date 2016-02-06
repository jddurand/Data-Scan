use strict;
use warnings FATAL => 'all';

package Data::Scan;
use Data::Scan::Impl::Option;
use Moo;
use Carp qw/croak/;
use Types::Standard qw/ConsumerOf/;

our $_defaultOption = Data::Scan::Impl::Option->new;

has option => (
               is => 'ro',
               isa => ConsumerOf['Data::Scan::Role::Option'],
               default => sub { $_defaultOption }
              );

sub process {
  my ($self) = shift;

  my $option = $self->option;
  while (@_) {
    $option->process($_[0]) if $option->wanted($_[0]);
    splice @_, 0, 1, $option->unfold(shift)
  }

  return
}

1;
