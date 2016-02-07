use strict;
use warnings FATAL => 'all';

package Data::Scan::Impl::_Default;
use Moo;
use Scalar::Util qw/refaddr reftype/;
use Types::Standard qw/HashRef Undef CodeRef Undef/;

# ABSTRACT: Data::Scan default implementation

# VERSION

# AUTHORITY

our %_UNFOLD = (
                ARRAY  => sub {                                     @{$_[0]} },
                HASH   => sub { map { $_ => $_[0]->{$_} } sort keys %{$_[0]} }
               );

has sort => (is => 'rw', isa => CodeRef|Undef, default => sub { return } );

sub start { $_[0]->{_seen} = {} }
sub end   { delete $_[0]->{_seen} }

sub _sort {
  # my ($self, $reftype, $ref) = @_;
  my $sort = $_[0]->sort;

  return $sort ? $sort->($_[2]) : $_UNFOLD{$_[1]}->($_[2])
}

sub process {
  # my ($self, $item) = @_;

  my @rc = ();
  if (my $reftype = reftype($_[1])) {
    if ($reftype eq 'ARRAY' || $reftype eq 'HASH') {  # Probably quicker than exists $_UNFOLD{$reftype}
      #
      # If this is is something that we can unfold, prevent recursion
      #
      my $seen = $_[0]->{_seen};
      my $refaddr = refaddr $_[1];
      @rc = $_[0]->_sort($reftype, $_[1]) if ! exists $seen->{$refaddr};
      $seen->{$refaddr} = 1
    } else {
      #
      # Dereference, whatever it is - the () are important to make sure
      # this is an array context
      #
      @rc = ( ${$_[1]} )
    }
  }

  @rc
}

sub push {}

sub pop {}

with 'Data::Scan::Role::Consumer';

1;
