use strict;
use warnings FATAL => 'all';

package Data::Scan::Impl::Printer;

# ABSTRACT: Data::Scan printer implementation

# VERSION

# AUTHORITY

use Data::Dumper;
use Moo;
use SUPER;
use Scalar::Util 1.26 qw/reftype refaddr blessed/;
use Term::ANSIColor;
use Types::Standard -all;
use Types::Common::Numeric -all;

#
# External attributes
#
has handle     => (is => 'ro', isa => FileHandle,       default => sub { return \*STDOUT  });
has indent     => (is => 'ro', isa => Str,              default => sub { return '  '      });
has undef      => (is => 'ro', isa => Str,              default => sub { return 'undef'   });
has unknown    => (is => 'ro', isa => Str,              default => sub { return '???'     });
has newline    => (is => 'ro', isa => Str,              default => sub { return "\n"      });
has ansicolor  => (is => 'ro', isa => Bool,             default => sub { return !!0       });
has colors     => (is => 'ro', isa => HashRef,          default => sub { return {
                                                                                 blessed => 'blue',
                                                                                 ref_start => 'yellow',
                                                                                 ref_end => 'yellow',
                                                                                 indice => 'magenta',
                                                                                 undef => 'red',
                                                                                 unknown => 'red',
                                                                                 adress => 'black',
                                                                                 string => 'cyan',
                                                                                 code    => 'green'
                                                                                }
                                                                       });
#
# Internal attributes
#
has _lines                  => (is => 'rw', isa => ArrayRef,                    clearer => 1, lazy => 1, default => sub { [ '' ] });
has _currentLevel           => (is => 'rw', isa => PositiveOrZeroInt,           clearer => 1, lazy => 1, default => sub { 0 });
has _currentIndicePerLevel  => (is => 'rw', isa => ArrayRef[PositiveOrZeroInt], clearer => 1, lazy => 1, default => sub { [] });
has _currentReftypePerLevel => (is => 'rw', isa => ArrayRef[Str],               clearer => 1, lazy => 1, default => sub { [] });
has _seen                   => (is => 'rw', isa => HashRef[PositiveOrZeroInt],  clearer => 1, lazy => 1, default => sub { {} });
#
# Required methods
#
sub start  {
  my ($self) = @_;
  $self->_clear_lines;
  $self->_clear_currentLevel;
  $self->_clear_currentIndicePerLevel;
  $self->_clear_currentReftypePerLevel;
  $self->_clear_seen;
}

sub end { !!1 }

sub output {
  my ($self) = @_;
  return join($self->newline, @{$self->_lines})
}

sub sopen {
  my ($self, $item) = @_;

  my $reftype = reftype $item;
  my $blessed = blessed $item;

  if    ($reftype eq 'ARRAY') { $self->_pushDesc('ref_start', '[')  }
  elsif ($reftype eq 'HASH')  { $self->_pushDesc('ref_start', '{')  }
  else                        { $self->_pushDesc('ref_start', '\\') }

  $self->_pushLevel($reftype);
  return
}

sub sclose {
  my ($self, $item) = @_;

  $self->_popLevel;

  my $reftype = reftype $item;
  if    ($reftype eq 'ARRAY') { $self->_pushLine; $self->_pushDesc('ref_end', ']') }
  elsif ($reftype eq 'HASH')  { $self->_pushLine; $self->_pushDesc('ref_end', '}') }

  return
}

sub sread {
  my ($self, $item) = @_;

  my $refaddr = refaddr($item);
  my $blessed = blessed($item) // '';
  my $reftype = reftype($item) // '';

  #
  # Push a newline and prefix with indice if in a fold
  #
  if ($self->_currentLevel) {
    my $currentReftypePerLevel = $self->_currentReftypePerLevel->[-1];
    my $currentIndicePerLevel = $self->_currentIndicePerLevel->[-1];
    if ($currentReftypePerLevel eq 'ARRAY' || $currentReftypePerLevel eq 'HASH') {
      $self->_pushLine;
      $self->_pushDesc('indice', "[$currentIndicePerLevel] ");
    }
    $self->_currentIndicePerLevel->[-1]++
  }
  #
  # See how this can be displayed
  #
  if ($blessed) {
    $self->_pushDesc('string', $blessed);
  } elsif (! $reftype) {
    if (defined($item)) {
      my $string = eval { "$item" };
      if (defined($string)) {
        $self->_pushDesc('string', $string);
      } else {
        $self->_pushDesc('string', $self->unknown);
      }
    } else {
      $self->_pushDesc('undef', $self->undef);
    }
  }
  #
  # Append adress to the name if already scanned
  #
  my $alreadyScanned;
  if ($refaddr) {
    my $hex = sprintf('0x%x', $refaddr);
    my $seen = $self->_seen;
    if (exists $seen->{$hex}) {
      $alreadyScanned = $hex;
    } else {
      $seen->{$hex} = 1
    }
    $self->_pushDesc('adress', "($hex)")
  }
  #
  # Blessed ?
  #
  $self->_pushDesc('blessed', '->') if $blessed;
  #
  # Already scanned ?
  #
  # $self->_pushDesc('adress', "*$alreadyScanned") if $alreadyScanned;
  #
  # Prepare return value
  #
  my $rc;
  if ($reftype && ! $alreadyScanned) {
    if ($reftype eq 'ARRAY') {
      $rc = $item
    } elsif ($reftype eq 'HASH') {
      $rc = [ map { $_ => $item->{$_} } sort { ($a // '') cmp ($b // '') } keys %{$item} ]
    } else {
      $rc = [ ${$item} ]
    }
  }

  $rc
}

#
# Internal methods
#
sub _pushLevel {
  my ($self, $reftype) = @_;

  push(@{$self->_currentReftypePerLevel}, $reftype);
  push(@{$self->_currentIndicePerLevel}, $reftype eq 'ARRAY' ? $[ : 0);       # Only used for ARRAY
  $self->_currentLevel($self->_currentLevel + 1);
  return
}

sub _popLevel {
  my ($self) = @_;

  pop(@{$self->_currentReftypePerLevel});
  pop(@{$self->_currentIndicePerLevel});
  $self->_currentLevel($self->_currentLevel - 1);
  return
}

sub _pushLine {
  my ($self) = @_;
  push(@{$self->_lines}, ($self->indent x $self->_currentLevel));
  return
}

sub _pushDesc {
  my ($self, $what, $desc) = @_;
  if ($self->ansicolor) {
    my $color = $self->colors->{$what};
    if ($color) {
      $self->_lines->[-1] .= colored($desc, $color);
    } else {
      $self->_lines->[-1] .= $desc;
    }
  } else {
    $self->_lines->[-1] .= $desc;
  }
  return
}

sub _currentDesc {
  my ($self) = @_;
  return $self->_lines->[-1]
}

with 'Data::Scan::Role::Consumer';

1;
