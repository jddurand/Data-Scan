use strict;
use warnings FATAL => 'all';

package Data::Scan::Printer;
use Exporter qw/import/;
use vars qw/@EXPORT/;
use Data::Scan::Impl::Printer;
use Moo;
extends 'Data::Scan';
#
# Using this module intentionnaly means caller is ok to pollute its namespace
#
@EXPORT = qw/dspp/;
#
# Convenient class method
#
sub dspp {
  my $consumer = Data::Scan::Impl::Printer->new(%Data::Scan::Printer::Option);
  __PACKAGE__->new(consumer => $consumer)->process(@_);
  $consumer->print
}

1;
