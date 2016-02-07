use strict;
use warnings FATAL => 'all';

package Data::Scan::Impl::Printer;

# ABSTRACT: Data::Scan printer implementation

# VERSION

# AUTHORITY

use Data::Dumper;
use Moo;
use SUPER;
use Scalar::Util qw/reftype blessed/;
use Types::Standard qw/FileHandle Str HashRef Bool/;

our %_FOLD   = ( SCALAR => '\\', ARRAY => '[', HASH => '{' );
our %_UNFOLD = ( SCALAR => '',   ARRAY => ']', HASH => '}' );

has handle    => (is => 'ro', isa => FileHandle,   default => sub { return \*STDOUT  });
has indent    => (is => 'ro', isa => Str,          default => sub { return '  '      });
has undef     => (is => 'ro', isa => Str,          default => sub { return 'undef'   });
has separator => (is => 'ro', isa => Str,          default => sub { return ' '       });
has unknown   => (is => 'ro', isa => Str,          default => sub { return '???'     });
has lf        => (is => 'ro', isa => Str,          default => sub { return "\n"      });
has fold      => (is => 'ro', isa => HashRef[Str], default => sub { return \%_FOLD   });
has unfold    => (is => 'ro', isa => HashRef[Str], default => sub { return \%_UNFOLD });
has color     => (is => 'ro', isa => Bool,         default => sub { return !!0       });

has output    => (is => 'rwp', isa => Str,         default => sub { ''               });

extends 'Data::Scan::Impl::_Default';

around start => sub {
  my ($orig, $self) = (shift, shift);
  $self->{_rc} = [];
  $self->{_level} = 0;
  $self->{_unfoldedIndice} = [];
  $self->{_unfoldedReftype} = [];
  $self->$orig(@_)
};

around end => sub {
  my ($orig, $self) = (shift, shift);
  $self->_set_output(join($self->lf, @{delete $self->{_rc}}));
  delete $self->{_level};
  delete $self->{_unfoldedIndice};
  delete $self->{_unfoldedReftype};
  $self->$orig(@_)
};

sub _indented {
  # my ($self, $currentString, $token) = @_;
  return ($_[0]->indent x $_[0]->{_level}) . $_[1]
}

sub _fold {
  # my ($self, $reftype) = @_;
  return $_[0]->fold->{$_[1]} // ''
}

sub _unfold {
  # my ($self, $reftype) = @_;
  return $_[0]->unfold->{$_[1]} // ''
}

sub push  {
  # my ($self, $item) = @_;
  my $reftype = reftype $_[1];
  #
  # We push a new line only if this is a HASH or an ARRAY reference
  #
  if ($reftype eq 'HASH' || $reftype eq 'ARRAY') {
    CORE::push(@{$_[0]->{_rc}}, $_[0]->_indented($_[0]->_fold($reftype)));
  }
  CORE::push(@{$_[0]->{_unfoldedIndice}}, $reftype eq 'ARRAY' ? $[ : 0);
  CORE::push(@{$_[0]->{_unfoldedReftype}}, $reftype);
  $_[0]->{_level}++;

  return
}
sub pop   {
  # my ($self, $item) = @_;
  my $reftype = reftype $_[1];
  $_[0]->{_level}--;

  if ($reftype eq 'HASH' || $reftype eq 'ARRAY') {
    CORE::push(@{$_[0]->{_rc}}, $_[0]->_indented($_[0]->_unfold($reftype)));
  }
  CORE::pop(@{$_[0]->{_unfoldedIndice}});
  CORE::pop(@{$_[0]->{_unfoldedReftype}});

  return
}
sub print {
  # my ($self) = @_;
  my $handle = $_[0]->handle;
  return CORE::print $handle $_[0]->output
}

around process => sub {
  my ($orig, $self, $item) = @_;

  my @description = ();
  #
  # Eventually append folded information
  #
  if ($self->{_level}) {
    if ($self->{_unfoldedReftype}->[-1] eq 'ARRAY') {
      CORE::push(@description, '[' . $self->{_unfoldedIndice}->[-1]++ . ']')
    } elsif ($self->{_unfoldedReftype}->[-1] eq 'HASH') {
      CORE::push(@description, '=>') if ($self->{_unfoldedIndice}->[-1]++ % 2)
    }
  }
  #
  # Get the stringified vision of item - this should rarelly fail
  # If it is blessed, use it, otherwise get its stringified version
  # if it is not a reference
  #
  my $stringified;
  if (my $blessed = blessed($item)) {
    $stringified = $blessed
  } elsif (! defined($item)) {
    $stringified = $self->undef
  } elsif (! ref($item)) {
    $stringified = do {eval { "$item" } // $self->unknown}
  } else {
    $stringified = '';
  }
  #
  # The unhandled case of unfolded SCALAR reference
  #
  if ($self->{_level} && ($self->{_unfoldedReftype}->[-1] ne 'ARRAY' && $self->{_unfoldedReftype}->[-1] ne 'HASH')) {
    CORE::push(@description, "\\$stringified");
  } else {
    CORE::push(@description, $stringified);
  }
  #
  # Push item description
  #
  my $description = join($self->separator, @description);
  if ($self->{_level} && $self->{_unfoldedReftype}->[-1] eq 'HASH') {
    if (! ($self->{_unfoldedIndice}->[-1] % 2)) {
      #
      # key => value is complete
      #
      CORE::push(@{$self->{_rc}}, $self->_indented(join($self->separator, delete($self->{_buffer}), $description)))
    } else {
      $self->{_buffer} = $description
    }
  } else {
    CORE::push(@{$self->{_rc}}, $self->_indented($description))
  }

  return $self->$orig($item);
};

1;
