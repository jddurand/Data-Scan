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
# Using this module intentionnaly means caller is ok to pollute its namespace
#
@EXPORT = qw/dspp/;
#
# Convenient class method
#
sub dspp {
  my $consumer = Data::Scan::Impl::Printer->new(%Data::Scan::Printer::Option);
  __PACKAGE__->new(consumer => $consumer)->process(@_);
  print $consumer->output
}

1;
