use strict;
use warnings FATAL => 'all';

package Data::Scan::Printer;

# ABSTRACT: Data::Scan::Printer - Example of a printer consumer for Data::Scan

# VERSION

# AUTHORITY

use Exporter qw/import/;
use vars qw/@EXPORT/;
use Data::Scan::Impl::Printer;
use Moo;
extends 'Data::Scan';
#
# Using this module intentionaly means caller is ok to pollute its namespace
#
@EXPORT = qw/dspp/;

=head1 DESCRIPTION

Data::Scan::Printer is polluting user's namespace with a dspp() method, showing our Data::Scan can be used to dump an arbitrary structure.

=head1 SYNOPSIS

    use strict;
    use warnings FATAL => 'all';
    use Data::Scan::Printer;

    my $this = bless([ 'var1', 'var2', {'a' => 'b', 'c' => 'd'}, \undef, \\undef, [] ], 'TEST');
    dspp($this);

=head1 SUBROUTINES/METHODS

=head2 dspp(@arguments)

Print to STDOUT a dumped vision of the arguments.

=cut

sub dspp {
  my $consumer = Data::Scan::Impl::Printer->new(%Data::Scan::Printer::Option);
  __PACKAGE__->new(consumer => $consumer)->process(@_);
  #
  # We know this consumer has a dsprint method
  #
  $consumer->dsprint
}

=head1 NOTES

If methods option is on, L<Class::Inspector> (and not L<Package::Stash> like what does L<Data::Printer>) is used to get public, private and other (labelled inherited, then) methods. Thus, notion of methods, usage of @ISA etc, could look different to what L<Data::Printer> say.

=head1 SEE ALSO

L<Data::Printer>, L<Class::Inspector>

=cut

1;
