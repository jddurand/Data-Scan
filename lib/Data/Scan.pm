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
    print $consumer->output;

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

=head2 $self->process(Any @arguments)

Scan over all items in @arguments and will call the consumer with these five methods/signatures:

=over

=item $consumer->start()

Indicates to the consumer that scanning is starting. Return value is ignored.

=item $consumer->sopen(Any $item)

Indicates to the consumer that an unfold of $item is starting. Return value is ignored.

=item $consumer->sread(Any $item)

Indicates to the consumer that he should take over $item. If the consumer is deciding to unfold it (typically when this is an ARRAY or a HASH reference), it should return an array reference containing the unfolded content. Anything but an an array reference means it has not been unfolded.

The consumer has full control on the workflow and can decide to unfold or not whatever is meaningful to him.

=item $consumer->sclose(Any $item)

Indicates to the consumer that an unfold of $item is ending. Return value is ignored.

=item $consumer->end()

Indicates to the consumer that scanning is ending. Return value of consumer->end() will be the return value of $self->process(@arguments).

=back

=cut

sub process {
  my ($self) = shift;

  my $consumer = $self->consumer;
  my $openaddr = \$_open;
  my $closeaddr = \$_close;

  my $previous;
  my $inner;
  my $reftype;

  #
  # Start
  #
  $consumer->start;                                                                      # start()
  while (@_) {
    #
    # First our private thingies
    #
    while (@_ && ref $_[$[]) {
      if    ($openaddr  == refaddr $_[$[]) { $consumer->sopen ((splice @_, $[, 2)[-1]) } # sopen($item)
      elsif ($closeaddr == refaddr $_[$[]) { $consumer->sclose((splice @_, $[, 2)[-1]) } # sclose($item)
      else                                 { last }
    }
    last if ! @_;
    #
    # Consumer's sread() returns eventual inner content
    #
    if (defined($inner = $consumer->sread($previous = shift))) {                       # sread($item)
      unshift(@_,
              $openaddr, $previous,
              @{$inner},
              $closeaddr, $previous
             ) if ((reftype($inner) // '') eq 'ARRAY')
    }
  }
  #
  # End - return value of consumer's end() is what we return
  #
  $consumer->end;                                                                        # end()
}

1;
