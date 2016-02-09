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
  my $readaddr = \$_read;
  my $closeaddr = \$_close;
  my %seen = ();

  my $previous;
  my @inner;
  my $reftype;

  #
  # Precompute some static information
  #
  my $secondIndice = $[+1;
  my $thirdIndice  = $[+2;
  my $fourthIndice = $[+3;
  my $nbElements = scalar(@_);
  #
  # Start
  #
  $consumer->start($nbElements);
  while (@_) {
    #
    # First our private thingies
    #
    while (ref $_[$[]) {
      if    ($openaddr  == refaddr $_[$[]) { $consumer->sopen ((splice @_, $[, 3)[$secondIndice..$thirdIndice]) } # sopen(item unfolded, number of elements)
      elsif ($readaddr  == refaddr $_[$[]) { $consumer->sread ((splice @_, $[, 3)[$secondIndice..$thirdIndice]) } # sread(item unfolded, item)
      elsif ($closeaddr == refaddr $_[$[]) { $consumer->sclose((splice @_, $[, 2)[$secondIndice])               } # sclose(item unfolded)
      else                                 { last }
    }
    if (@_) {
      #
      # Prevent infinite recursion
      #
      if ($reftype = reftype $_[$[]) {
        my $refaddr = refaddr $_[$[];
        if (exists $seen{$refaddr}) {
          shift, next
        }
        $seen{$refaddr} = 1
      }
      #
      # Consumer's sread() returns eventual inner content
      #
      if (@inner = $consumer->sread(undef, $previous = shift)) { # sread(item, undef)
        unshift(@_,
                $openaddr, $previous, scalar(@inner),
                (map { $readaddr, $previous, $inner[$_] } $[..$#inner),
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
  $consumer->end($nbElements);
}

1;
