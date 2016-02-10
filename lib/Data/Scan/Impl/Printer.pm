use strict;
use warnings FATAL => 'all';

package Data::Scan::Impl::Printer;

# ABSTRACT: Data::Scan printer implementation

# VERSION

# AUTHORITY

use Data::Dumper;
use IO::Interactive::Tiny;
use Moo;
use Perl::OSType qw/is_os_type/;
my $_HAVE_Win32__Console__ANSI;
BEGIN {
  #
  # Will/Should success only on Win32
  #
  $_HAVE_Win32__Console__ANSI = eval 'use Win32::Console::ANSI; 1;' ## no critic qw/BuiltinFunctions::ProhibitStringyEval/
}
use Scalar::Util 1.26 qw/reftype refaddr looks_like_number/;
use Term::ANSIColor;
use Types::Standard -all;
use Types::Common::Numeric -all;
#
# Can ansi colors be used ?
#
my $_CAN_COLOR = IO::Interactive::Tiny::is_interactive() ? (is_os_type('Windows') ? $_HAVE_Win32__Console__ANSI : 1) : 0;
#
# My way of matching only printable ASCII characters
#
my $_ASCII_PRINT = quotemeta(join('', map { chr } 33..126));
my $_NON_ASCII_PRINT_RE = qr/[^$_ASCII_PRINT]/;
#
# External attributes
#
has handle            => (is => 'ro', isa => FileHandle,       default => sub { return \*STDOUT  });
has indent            => (is => 'ro', isa => Str,              default => sub { return '  '      });
has undef             => (is => 'ro', isa => Str,              default => sub { return 'undef'   });
has unknown           => (is => 'ro', isa => Str,              default => sub { return '???'     });
has newline           => (is => 'ro', isa => Str,              default => sub { return "\n"      });
has ansicolor         => (is => 'ro', isa => Bool,             default => sub { return !!1       });
has array_start       => (is => 'ro', isa => Str,              default => sub { return ' ['       });
has array_end         => (is => 'ro', isa => Str,              default => sub { return ']'       });
has hash_start        => (is => 'ro', isa => Str,              default => sub { return ' {'       });
has hash_end          => (is => 'ro', isa => Str,              default => sub { return '}'       });
has hash_separator    => (is => 'ro', isa => Str,              default => sub { return ' => '    });
has indice_start      => (is => 'ro', isa => Str,              default => sub { return '['       });
has indice_end        => (is => 'ro', isa => Str,              default => sub { return '] '      });
has address_start     => (is => 'ro', isa => Str,              default => sub { return '('       });
has address_end       => (is => 'ro', isa => Str,              default => sub { return ')'       });
has blessed           => (is => 'ro', isa => Str,              default => sub { return ''        });
has ref_start         => (is => 'ro', isa => Str,              default => sub { return '\\'      });
has show_address      => (is => 'ro', isa => Bool,             default => sub { return !!0       });
has show_array_indice => (is => 'ro', isa => Bool,             default => sub { return !!1       });
has show_hash_indice  => (is => 'ro', isa => Bool,             default => sub { return !!0       });
has colors            => (is => 'ro', isa => HashRef,          default => sub { return {
                                                                                        # string          => '',
                                                                                        # blessed         => 'magenta',
                                                                                        array_start     => 'bold yellow',
                                                                                        array_end       => 'bold yellow',

                                                                                        hash_start      => 'bold yellow',
                                                                                        hash_key        => 'bold yellow',
                                                                                        hash_separator  => 'magenta',
                                                                                        hash_value      => 'bold yellow',
                                                                                        hash_end        => 'bold yellow',

                                                                                        ref_start       => 'bold yellow',

                                                                                        indice_start    => 'magenta',
                                                                                        indice_value    => 'magenta',
                                                                                        indice_end      => 'magenta',

                                                                                        undef           => 'bold red',
                                                                                        unknown         => 'bold red',
                                                                                        address         => ['magenta on_black'],
                                                                                        code            => 'green',
                                                                                        already_scanned => 'blink green',
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
  my $blessed = Scalar::Util::blessed $item;

  if    ($reftype eq 'ARRAY') { $self->_pushDesc('array_start', $self->array_start) }
  elsif ($reftype eq 'HASH')  { $self->_pushDesc('hash_start',  $self->hash_start)  }
  else                        { $self->_pushDesc('ref_start',   $self->ref_start)   }

  $self->_pushLevel($reftype);
  return
}

sub sclose {
  my ($self, $item) = @_;

  $self->_popLevel;

  my $reftype = reftype $item;
  if    ($reftype eq 'ARRAY') { $self->_pushLine; $self->_pushDesc('array_end', $self->array_end) }
  elsif ($reftype eq 'HASH')  { $self->_pushLine; $self->_pushDesc('hash_end', $self->hash_end)   }

  return
}

sub sread {
  my ($self, $item) = @_;

  my $refaddr = refaddr($item);
  my $blessed = Scalar::Util::blessed($item) // '';
  my $reftype = reftype($item) // '';

  #
  # Push a newline or a '=>' and prefix with indice if in a fold
  #
  my $currentLevel = $self->_currentLevel;
  if ($currentLevel) {
    my $currentReftypePerLevel = $self->_currentReftypePerLevel->[-1];
    my $currentIndicePerLevel = $self->_currentIndicePerLevel->[-1];
    if ($currentReftypePerLevel eq 'ARRAY' or $currentReftypePerLevel eq 'HASH') {
      my $show_indice;
      if ($currentReftypePerLevel eq 'ARRAY') {
        $self->_pushLine;
        $show_indice = $self->show_array_indice;
      } else {
        if ($currentIndicePerLevel % 2) {
          $self->_pushDesc('hash_separator', $self->hash_separator);
        } else {
          $self->_pushLine;
        }
        $show_indice = $self->show_hash_indice;
      }
      if ($show_indice) {
        $self->_pushDesc('indice_start', $self->indice_start);
        $self->_pushDesc('indice_value', $currentIndicePerLevel);
        $self->_pushDesc('indice_end', $self->indice_end);
      }
    }
    $self->_currentIndicePerLevel->[-1]++
  }
  #
  # See how this can be displayed
  #
  my $alreadyScanned;
  my $hex;
  if ($refaddr) {
    $hex = sprintf('0x%x', $refaddr);
    my $seen = $self->_seen;
    if (exists $seen->{$hex}) {
      $alreadyScanned = $seen->{$hex};
      #
      # Already scanned !
      #
      $self->_pushDesc('already_scanned', $alreadyScanned);
    } else {
      #
      # Determine the "location" in terms of an hypothetical "@var" describing the tree
      #
      my @levels = ();
      my $level = $currentLevel;
      while ($level-- > 0) {
        unshift(@levels, '[' . ($self->_currentIndicePerLevel->[$level] - 1) . ']');
      }
      $seen->{$hex} = 'var' . join('', @levels);
    }
  }
  if (! $alreadyScanned) {
    if ($blessed) {
      #
      # Priority is given to blessed name
      #
      $self->_pushDesc('string', $blessed);
    } elsif (! $reftype) {
      #
      # Otherwise stringify if possible
      #
      if (defined($item)) {
        my $string = eval { "$item" }; ## no critic qw/BuiltinFunctions::ProhibitStringyEval/
        if (defined($string)) {
          $self->_pushDesc('string', $string);
        } else {
          $self->_pushDesc('unknown', $self->unknown);
        }
      } else {
        $self->_pushDesc('undef', $self->undef);
      }
    }
    #
    # Show address ?
    #
    if ($refaddr && $self->show_address) {
      $self->_pushDesc('address_start', $self->address_start);
      $self->_pushDesc('address', $hex);
      $self->_pushDesc('address_end', $self->address_end);
    }
    #
    # Show blessed indication ?
    #
    $self->_pushDesc('blessed', $self->blessed) if $blessed;
  }
  #
  # Eventually increase indice number
  #
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

  if ($what eq 'string') {
    #
    # Detect any non ANSI character and enclose within ""
    #
    $desc =~ s/$_NON_ASCII_PRINT_RE/sprintf('\\x{%x}', ord(${^MATCH}))/egp;
    $desc = "\"$desc\"" unless looks_like_number($desc);
  }

  if ($_CAN_COLOR && $self->ansicolor) {
    my $color = $self->colors->{$what};
    if ($color) {
      my $refcolor = reftype($color);
      if (! $refcolor) {
        $self->_lines->[-1] .= colored($desc, $color);
      } elsif ($refcolor eq 'ARRAY') {
        $self->_lines->[-1] .= colored($color, $desc);
      } else {
        $self->_lines->[-1] .= $desc;
      }
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
