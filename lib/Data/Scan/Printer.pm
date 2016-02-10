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

=head1 SUBROUTINES/METHODS

=head2 dspp(@arguments)

Print to STDOUT a dumped vision of the arguments.

=cut

sub dspp {
  my $consumer = Data::Scan::Impl::Printer->new(%Data::Scan::Printer::Option);
  __PACKAGE__->new(consumer => $consumer)->process(@_);
  print $consumer->output
}

1;
