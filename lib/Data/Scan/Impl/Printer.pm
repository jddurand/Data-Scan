use strict;
use warnings FATAL => 'all';

package Data::Scan::Impl::Printer;

# ABSTRACT: Data::Scan printer implementation

# VERSION

# AUTHORITY

use Data::Dumper;
use Moo;
use SUPER;
use Scalar::Util 1.26 qw/reftype blessed/;
use Types::Standard -all;
use Types::Common::Numeric -all;

has handle     => (is => 'ro', isa => FileHandle,       default => sub { return \*STDOUT  });
has indent     => (is => 'ro', isa => Str,              default => sub { return '  '      });
has undef      => (is => 'ro', isa => Str,              default => sub { return 'undef'   });
has unknown    => (is => 'ro', isa => Str,              default => sub { return '???'     });
has newline    => (is => 'ro', isa => Str,              default => sub { return "\n"      });
has color      => (is => 'ro', isa => Bool,             default => sub { return !!0       });

has item_start    => (is => 'ro', isa => CodeRef, default => sub { \&_item_start });
has item_unfold   => (is => 'ro', isa => CodeRef, default => sub { \&_item_unfold });
has item_nextfold => (is => 'ro', isa => CodeRef, default => sub { \&_item_nextfold });
has item_end      => (is => 'ro', isa => CodeRef, default => sub { \&_item_end });
#
# Internal attributes
#
has _lines                  => (is => 'rw', isa => ArrayRef,                    clearer => 1, lazy => 1, default => sub { [ '' ] });
has _currentLevel           => (is => 'rw', isa => PositiveOrZeroInt,           clearer => 1, lazy => 1, default => sub { 0 });
has _currentIndicePerLevel  => (is => 'rw', isa => ArrayRef[PositiveOrZeroInt], clearer => 1, lazy => 1, default => sub { [] });
has _currentReftypePerLevel => (is => 'rw', isa => ArrayRef[Str],               clearer => 1, lazy => 1, default => sub { [] });
#
# Internal methods
#
sub _pushLine {
  my ($self, $desc) = @_;
  push(@{$self->_lines}, ($self->indent x $self->_currentLevel) . $desc);
  return
}

sub _pushDesc {
  my ($self, $desc) = @_;
  $self->_lines->[-1] .= $desc;
  return
}
#
# Default callbacks
#
sub _item_start {
  my ($self, $item) = @_;

  my $header = '';
  if ($self->_currentLevel) {
    if ($self->_currentReftypePerLevel->[-1] eq 'ARRAY') {
      my $currentIndicePerLevel = $self->_currentIndicePerLevel->[-1];
      $header = "[$currentIndicePerLevel] ";
      $self->_currentIndicePerLevel->[-1] = $currentIndicePerLevel + 1;
    }
  }

  if (my $reftype = reftype $item) {
    my $blessed = blessed($item) // '';
    if    ($reftype eq 'ARRAY') { $self->_pushLine("$header$blessed\[") }
    elsif ($reftype eq 'HASH')  { $self->_pushLine("$header$blessed\{") }
    else                        { $self->_pushDesc("$header\\$blessed") }
  } else {
    if (defined($item)) {
      $self->_pushDesc($header . do { eval { "$item" } // $self->unknown })
    } else {
      $self->_pushDesc($header . $self->undef )
    }
  }
  return;
}

sub _item_unfold {
  my ($self, $item) = @_;

  my @unfold = ();
  my $reftype = reftype($item);

  if ($reftype) {
    if    ($reftype eq 'ARRAY') { @unfold = @{$item} }
    elsif ($reftype eq 'HASH')  { @unfold = sort { ($a // '') cmp ($b // '')} keys %{$item} }
    else                        { @unfold = ${$_[1]} }
    push(@{$self->_currentReftypePerLevel}, $reftype);
    push(@{$self->_currentIndicePerLevel}, $[);       # Only used for ARRAY
    $self->_currentLevel($self->_currentLevel + 1);
  }
  @unfold
}

sub _item_nextfold {
  my ($self, $item) = @_;

  $self->_pushLine('');
  return
}

sub _item_end {
  my ($self, $item) = @_;

  if (my $reftype = reftype($item)) {
    #
    # We unfolded only if reftype is a true value
    #
    if    ($reftype eq 'ARRAY') { $self->_pushLine("\]") }
    elsif ($reftype eq 'HASH')  { $self->_pushLine("\}") }
    else                        {                        }
  }
  pop(@{$self->_currentReftypePerLevel});
  pop(@{$self->_currentIndicePerLevel});
  $self->_currentLevel($self->_currentLevel - 1);
  return
}

sub start  {
  my ($self) = @_;
  $self->_clear_lines;
  $self->_clear_currentLevel;
  $self->_clear_currentIndicePerLevel;
  $self->_clear_currentReftypePerLevel;
}



sub output {
  my ($self) = @_;
  return join($self->newline, @{$self->_lines})
}

sub endfold {
  my ($self, $item) = @_;
  return $self->item_end->($self, $item)
}

sub nextfold {
  my ($self, $item) = @_;
  return $self->item_nextfold->($self, $item)
}

sub end { return }

sub process {
  my ($self, $item) = @_;

  $self->_item_start($item);
  return $self->item_unfold->($self, $item);
}

with 'Data::Scan::Role::Consumer';

1;
