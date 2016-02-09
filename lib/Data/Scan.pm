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

our $_open;
our $_read;
our $_close;

has consumer => (
                 is => 'ro',
                 isa => ConsumerOf['Data::Scan::Role::Consumer'],
                );

sub process {
  my ($self) = shift;

  my $consumer = $self->consumer;
  my $openaddr = \$_open;
  my $closeaddr = \$_close;

  my $previous;
  my @inner;
  my $reftype;

  #
  # Start
  #
  $consumer->start();
  while (@_) {
    #
    # First our private thingies
    #
    while (ref $_[$[]) {
      if    ($openaddr  == refaddr $_[$[]) { $consumer->sopen ((splice @_, $[, 2)[-1]) } # sopen(item)
      elsif ($closeaddr == refaddr $_[$[]) { $consumer->sclose((splice @_, $[, 2)[-1]) } # sclose(item)
      else                                 { last }
    }
    if (@_) {
      #
      # Consumer's sread() returns eventual inner content
      #
      if (@inner = $consumer->sread($previous = shift)) { # sread(item, undef)
        unshift(@_,
                $openaddr, $previous,
                @inner,
                $closeaddr, $previous
               )
      }
    } else {
      last
    }
  }
  #
  # End - return value of consumer's end() is what we return
  #
  $consumer->end();
}

1;
