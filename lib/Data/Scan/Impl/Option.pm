use strict;
use warnings FATAL => 'all';

package Data::Scan::Impl::Option;
use Moo;
use Scalar::Util qw/refaddr reftype/;
use Types::Standard qw/HashRef/;

has _seen => (is => 'rw',
              isa => HashRef,  # Avoid MooX::HandlesVia for performance
              default => sub { {} }
             );

#
# For performance we work directly on the stack in the methods below
#
sub wanted {
  my $seen = $_[0]->_seen;

  if (my $refaddr = refaddr $_[1]) {    # A reference ?
    return if exists $seen->{$refaddr}; # Prevent loop
    $seen->{$refaddr} = 1;              # ./.. and remember it
  }

  1
}

sub unfold {

  my $reftype = reftype $_[1];

  my @rc = ();
  if (defined $reftype) {
    if ($reftype eq 'SCALAR')   { @rc = ${$_[1]} }
    elsif ($reftype eq 'ARRAY') { @rc = @{$_[1]} }
    elsif ($reftype eq 'HASH')  { @rc =  keys %{$_[1]} }         # No sort
  }
  @rc
}

sub process {}

with 'Data::Scan::Role::Option';

1;
