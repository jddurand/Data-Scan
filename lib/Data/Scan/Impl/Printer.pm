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
has color      => (is => 'ro', isa => Bool,             default => sub { return !!0       });
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

  if    ($blessed)            { $self->_pushDesc('->') }

  if    ($reftype eq 'ARRAY') { $self->_pushDesc('['); $self->_pushLevel($reftype) }
  elsif ($reftype eq 'HASH')  { $self->_pushDesc('{'); $self->_pushLevel($reftype) }
  else                        { $self->_pushDesc('\\');  $self->_pushLevel($reftype) }

  return
}

sub sclose {
  my ($self, $item) = @_;

  $self->_popLevel;

  my $reftype = reftype $item;
  if    ($reftype eq 'ARRAY') { $self->_pushLine; $self->_pushDesc(']') }
  elsif ($reftype eq 'HASH')  { $self->_pushLine; $self->_pushDesc('}') }
  else                        {                                         }

  return
}

sub sread {
  my ($self, $item) = @_;

  my $refaddr = refaddr($item);
  my $blessed = blessed($item) // '';
  my $reftype = reftype($item) // '';
  my @desc = ();
  my $pushLine = 0;

  my $name;
  if ($blessed) {
    $name = $blessed
  } elsif (! $reftype) {
    #
    # Stringification
    #
    $name = defined($item) ? do { eval { "$item" } // $self->unknown } : $self->undef
  }
  #
  # Already scanned ?
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
  }
  $name .= "($alreadyScanned)" if $alreadyScanned;

  if ($self->_currentLevel) {
    my $currentReftypePerLevel = $self->_currentReftypePerLevel->[-1];
    my $currentIndicePerLevel = $self->_currentIndicePerLevel->[-1];
    if ($currentReftypePerLevel eq 'ARRAY') {
      $self->_pushDesc(',') if ($currentIndicePerLevel > $[);
      $self->_pushLine;
      push(@desc, "[$currentIndicePerLevel]");
      push(@desc, $name) if $name;
      $self->_pushDesc(join(' ', @desc));
    } elsif ($currentReftypePerLevel eq 'HASH') {
      if ($currentIndicePerLevel % 2) {
        push(@desc, $name) if $name;
        $self->_pushDesc(join(' ', @desc));
      } else {
        $self->_pushDesc(',') if ($currentIndicePerLevel >= 2);
        $self->_pushLine;
        push(@desc, $name) if $name;
        push(@desc, '=>');
        push(@desc, '');
        $self->_pushDesc(join(' ', @desc));
      }
    } else {
      push(@desc, $name) if $name;
      $self->_pushDesc(join(' ', @desc));
    }
    $self->_currentIndicePerLevel->[-1]++
  } else {
    push(@desc, $name) if $name;
    $self->_pushDesc(join(' ', @desc));
  }
  #
  # Unfold
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
  my ($self, $desc) = @_;
  $self->_lines->[-1] .= $desc;
  return
}

with 'Data::Scan::Role::Consumer';

1;
