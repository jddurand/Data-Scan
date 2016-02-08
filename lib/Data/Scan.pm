use strict;
use warnings FATAL => 'all';

package Data::Scan;
use Carp qw/croak/;
use Moo;
use Scalar::Util qw/refaddr reftype/;
use Types::Standard qw/ConsumerOf/;

# ABSTRACT: Data::Scan

# VERSION

# AUTHORITY

our $_endfold;
our $_nextfold;

has consumer => (
                 is => 'ro',
                 isa => ConsumerOf['Data::Scan::Role::Consumer'],
                );

sub process {
  my ($self) = shift;

  my $consumer = $self->consumer;
  my $endfoldaddr = \$_endfold;
  my $nextfoldaddr = \$_nextfold;
  my %seen = ();

  my $previous;
  my @unfold;
  my $reftype;

  $consumer->start;
  while (@_) {
    #
    # Consume first all our private thingies
    #
    while (ref $_[0]) {
      if ($endfoldaddr == refaddr $_[0]) {
        $consumer->endfold((splice @_, 0, 2)[1])
      } elsif ($nextfoldaddr == refaddr $_[0]) {
        $consumer->nextfold((splice @_, 0, 2)[1])
      } else {
        last
      }
    }
    if (@_) {
      #
      # Prevent infinite recursion
      #
      if ($reftype = reftype $_[0]) {
        my $refaddr = refaddr $_[0];
        if (exists $seen{$refaddr}) {
          shift, next
        }
        $seen{$refaddr} = 1
      }
      #
      # Process returns eventual unfolded content
      #
      if (@unfold = $consumer->process($previous = shift)) {
        unshift(@_, (map { $nextfoldaddr, $previous, $unfold[$_] } 0..$#unfold), $endfoldaddr, $previous)
      }
    } else {
      last
    }
  }
  $consumer->end
}

1;
