use strict;
use warnings FATAL => 'all';

package Data::Scan;
use Carp qw/croak/;
use Data::Scan::Impl::_Default;
use Moo;
use Scalar::Util qw/refaddr/;
use Types::Standard qw/ConsumerOf/;

# ABSTRACT: Data::Scan

# VERSION

# AUTHORITY

our $_start;
our $_end;

has consumer => (
                 is => 'ro',
                 isa => ConsumerOf['Data::Scan::Role::Consumer'],
                 default => sub { Data::Scan::Impl::_Default->new }
                );

sub process {
  my ($self) = shift;

  my $consumer = $self->consumer;
  my $startaddr = \$_start;
  my $endaddr = \$_end;

  my $previous;
  my @unfold;
  $consumer->start;
  while (@_) {
    #
    # Consume first all our private thingies
    #
    while (ref($_[0])) {
      if    ($startaddr == refaddr($_[0])) { $consumer->push((splice(@_, 0, 2))[1]) }
      elsif ($endaddr   == refaddr($_[0])) { $consumer->pop((splice(@_, 0, 2))[1]) }
      else                                 { last }
    }
    if (@_) {
      if (@unfold = $consumer->process($previous = shift)) {
        unshift(@_, $startaddr, $previous, @unfold, $endaddr, $previous)
      }
    } else {
      last
    }
  }
  $consumer->end;

  return
}

1;
