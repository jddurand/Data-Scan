use strict;
use warnings FATAL => 'all';

package Data::Scan::Printer;

# ABSTRACT: Example of a printer consumer for Data::Scan

# VERSION

# AUTHORITY

use Exporter qw/import/;
use vars qw/@EXPORT %Option/;
use Data::Scan::Impl::Printer;
use Moo;
extends 'Data::Scan';
#
# Using this module intentionaly means caller is ok to pollute its namespace
#
@EXPORT = qw/dspp/;
#
# User is advised to localized that
#
%Option = ();

=head1 DESCRIPTION

Data::Scan::Printer is polluting user's namespace with a dspp() method, showing how Data::Scan can be used to dump an arbitrary structure. This is a sort of L<Data::Printer> alternative.

=head1 SYNOPSIS

    use strict;
    use warnings FATAL => 'all';
    use Data::Scan::Printer;

    my $this = bless([ 'var1', 'var2', {'a' => 'b', 'c' => 'd'}, \undef, \\undef, [], sub { return 'something' } ], 'TEST');
    local %Data::Scan::Printer::Option = (with_deparse => 1);
    dspp($this);

=head1 SUBROUTINES/METHODS

=head2 dspp(@arguments)

Print to Data::Scan::Impl::Printer's handle a dumped vision of @arguments. An instance of Data::Scan::Impl::Printer is created automatically, using parameters that have to be available in %Data::Scan::Printer::Option. Please refer to L<Data::Scan::Impl::Printer> documentation for the available options.

=cut

sub dspp {
  my $consumer = Data::Scan::Impl::Printer->new(%Option);
  __PACKAGE__->new(consumer => $consumer)->process(@_);
  #
  # We know this consumer has a dsprint method
  #
  $consumer->dsprint
}

=head1 SEE ALSO

L<Data::Scan>, L<Data::Scan::Impl::Printer>

=cut

1;
