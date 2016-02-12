use strict;
use warnings FATAL => 'all';

package Data::Scan;

# ABSTRACT: Stackfree arbitrary data scanner

# VERSION

# AUTHORITY

use Carp qw/croak/;
use Moo;
use Scalar::Util qw/refaddr reftype/;
use Types::Standard qw/ConsumerOf/;

=head1 DESCRIPTION

Data::Scan is a stackfree scanner of arbitrary data. It has no other intelligence but scanning its arguments and asking for a consumer to deal with every item.

=head1 SYNOPSIS

    use strict;
    use warnings FATAL => 'all';
    use Data::Scan;
    use Data::Scan::Impl::Printer;  # Basic example that is doing an indented print

    my $this = bless([ 'var1', 'var2', {'a' => 'b', 'c' => 'd'}, \undef, \\undef, [] ], 'TEST');
    my $consumer = Data::Scan::Impl::Printer->new;
    Data::Scan->new(consumer => $consumer)->process($this);

=cut

has consumer => (
                 is => 'ro',
                 isa => ConsumerOf['Data::Scan::Role::Consumer'],
                );

=head1 SUBROUTINES/METHODS

=head2 $class->new(consumer => ConsumerOf['Data::Scan::Role::Consumer'])

Instantiate a new Data::Scan object. Takes as parameter a required consumer, that is consuming the Data::Scan::Role::Consumer role.

=cut

my $_open;
my $_close;
my $openaddr = \$_open;
my $closeaddr = \$_close;

=head2 $self->process(Any @arguments)

Scan over all items in @arguments and will call the consumer with these five methods/signatures:

=over

=item $consumer->dsstart()

Indicates to the consumer that scanning is starting. Return value is ignored.

=item $consumer->dsopen(Any $item)

Indicates to the consumer that an unfold of $item is starting. Return value is ignored.

=item $consumer->dsread(Any $item)

Indicates to the consumer that he should take over $item. If the consumer is deciding to unfold it (typically when this is an ARRAY or a HASH reference), it should return an array reference containing the unfolded content. Anything but an an array reference means it has not been unfolded.

The consumer has full control on the workflow and can decide to unfold or not whatever is meaningful to him.

=item $consumer->dsclose(Any $item)

Indicates to the consumer that an unfold of $item is ending. Return value is ignored.

=item $consumer->dsend()

Indicates to the consumer that scanning is ending. Return value of consumer->end() will be the return value of $self->process(@arguments).

=back

=cut

sub process {
  my ($self) = shift;

  my ($consumer, $previous, $inner) = $self->consumer;
  #
  # Start
  #
  $consumer->dsstart();
  #
  # Loop
  #
  while (@_) {
    #
    # First our private thingies
    #
    while (@_ && ref $_[$[]) {
      if    ($openaddr  == refaddr $_[$[]) { $consumer->dsopen ((splice @_, $[, 2)[-1]) }
      elsif ($closeaddr == refaddr $_[$[]) { $consumer->dsclose((splice @_, $[, 2)[-1]) }
      else                                 { last }
    }
    #
    # Consumer's dsread() returns eventual inner content
    #
    unshift(@_,
            $openaddr, $previous,
            @{$inner},
            $closeaddr, $previous
           ) if (@_ && defined($inner = $consumer->dsread($previous = shift)) && (reftype($inner) // '') eq 'ARRAY')
  }
  #
  # End - return value of consumer's dsend() is what we return
  #
  return $consumer->dsend()
}

1;
