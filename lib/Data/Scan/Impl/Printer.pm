use strict;
use warnings FATAL => 'all';

package Data::Scan::Impl::Printer;

# ABSTRACT: Data::Scan printer implementation

# VERSION

# AUTHORITY

use B::Deparse;
use Class::Inspector;
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
has array_start       => (is => 'ro', isa => Str,              default => sub { return ' ['      });
has array_next        => (is => 'ro', isa => Str,              default => sub { return ','       });
has array_end         => (is => 'ro', isa => Str,              default => sub { return ']'       });
has hash_start        => (is => 'ro', isa => Str,              default => sub { return ' {'      });
has hash_end          => (is => 'ro', isa => Str,              default => sub { return '}'       });
has hash_next         => (is => 'ro', isa => Str,              default => sub { return ','       });
has hash_separator    => (is => 'ro', isa => Str,              default => sub { return ' => '    });
has indice_start      => (is => 'ro', isa => Str,              default => sub { return '['       });
has indice_end        => (is => 'ro', isa => Str,              default => sub { return '] '      });
has address_start     => (is => 'ro', isa => Str,              default => sub { return '('       });
has address_end       => (is => 'ro', isa => Str,              default => sub { return ')'       });
has blessed           => (is => 'ro', isa => Str,              default => sub { return ''        });
has ref_start         => (is => 'ro', isa => Str,              default => sub { return '\\'      });
has show_address      => (is => 'ro', isa => Bool,             default => sub { return !!0       });
has show_array_indice => (is => 'ro', isa => Bool,             default => sub { return !!1       });
has show_hash_indice  => (is => 'ro', isa => Bool,             default => sub { return !!1       });
has indices_full      => (is => 'ro', isa => Bool,             default => sub { return !!1       });
has deparse           => (is => 'ro', isa => Bool,             default => sub { return !!0       });
has methods           => (is => 'ro', isa => Bool,             default => sub { return !!1       });
has colors            => (is => 'ro', isa => HashRef,          default => sub { return {
                                                                                        # string          => 'green',
                                                                                        # blessed         => 'magenta',
                                                                                        array_start     => 'magenta',
                                                                                        array_next      => 'magenta',
                                                                                        array_end       => 'magenta',

                                                                                        hash_start      => 'magenta',
                                                                                        hash_separator  => 'magenta',
                                                                                        hash_next       => 'magenta',
                                                                                        hash_end        => 'magenta',

                                                                                        # ref_start       => 'bold yellow',

                                                                                        indice_full     => 'magenta',
                                                                                        indice_start    => 'magenta',
                                                                                        indice_value    => 'magenta',
                                                                                        indice_end      => 'magenta',

                                                                                        undef           => 'red',
                                                                                        unknown         => 'bold red',
                                                                                        address_start   => 'magenta',
                                                                                        address         => 'magenta',
                                                                                        address_end     => 'magenta',
                                                                                        code            => 'yellow',
                                                                                        already_scanned => 'blink green',
                                                                                        deparse         => 'green',
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
sub dsstart  {
  my ($self) = @_;
  $self->_clear_lines;
  $self->_clear_currentLevel;
  $self->_clear_currentIndicePerLevel;
  $self->_clear_currentReftypePerLevel;
  $self->_clear_seen;
}

sub dsend { !!1 }

sub dsprint {
  my ($self) = @_;

  my $handle = $self->handle;
  my $string = join($self->newline, @{$self->_lines});
  if (Scalar::Util::blessed($handle) && $handle->can('print')) {
    return $handle->print($string)
  } else {
    return print $handle $string
  }
}

sub dsopen {
  my ($self, $item) = @_;

  my $reftype = reftype $item;
  my $blessed = Scalar::Util::blessed $item;

  if    ($reftype eq 'ARRAY') { $self->_pushDesc('array_start', $self->array_start) }
  elsif ($reftype eq 'HASH')  { $self->_pushDesc('hash_start',  $self->hash_start)  }
  else                        { $self->_pushDesc('ref_start',   $self->ref_start)   }

  $self->_pushLevel($reftype);
  return
}

sub dsclose {
  my ($self, $item) = @_;

  $self->_popLevel;

  my $reftype = reftype $item;
  if    ($reftype eq 'ARRAY') { $self->_pushLine; $self->_pushDesc('array_end', $self->array_end) }
  elsif ($reftype eq 'HASH')  { $self->_pushLine; $self->_pushDesc('hash_end', $self->hash_end)   }

  return
}

sub dsread {
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
        $self->_pushDesc('array_next', $self->array_next) if ($currentIndicePerLevel > $[);
        $self->_pushLine;
        $show_indice = $self->show_array_indice
      } else {
        if ($currentIndicePerLevel % 2) {
          $self->_pushDesc('hash_separator', $self->hash_separator);
        } else {
          $self->_pushDesc('hash_next', $self->hash_next) if ($currentIndicePerLevel > 0);
          $self->_pushLine;
        }
        $show_indice = $self->show_hash_indice
      }
      if ($show_indice) {
        if ($self->indices_full) {
          my @levels = (
                        $self->_concatenateLevels($currentLevel - 1),
                        $self->indice_start . $currentIndicePerLevel . $self->indice_end
                       );
          #
          # As in a \var reference (see below), we want the levels to not have eventual space
          #
          my $levels = join('', @levels);
          $levels =~ s/\s//g;
          $self->_pushDesc('indice_full', $levels)
        } else {
          $self->_pushDesc('indice_start', $self->indice_start);
          $self->_pushDesc('indice_value', $currentIndicePerLevel);
          $self->_pushDesc('indice_end', $self->indice_end)
        }
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
      $self->_pushDesc('already_scanned', $alreadyScanned)
    } else {
      #
      # Determine the "location" in terms of an hypothetical "@var" describing the tree
      # Note that we already increased $currentLevel
      my @levels = $self->_concatenateLevels($currentLevel);
      #
      # We want the levels to not have eventual space
      #
      my $levels = join('', @levels);
      $levels =~ s/\s//g;
      $seen->{$hex} = "var$levels"
    }
  }
  if (! $alreadyScanned) {
    if ($blessed && $reftype ne 'REGEXP') {
      #
      # A regexp appears as being blessed in perl.
      # Priority is given to blessed name except if it is a regexp.
      #
      $self->_pushDesc('string', $blessed)
    } elsif ($reftype eq 'CODE' && $self->deparse) {
      #
      # Code deparse with B::Deparse
      #
      my $i = length($self->indent) x ($self->_currentLevel + 2);
      my $deparseopts = ["-sCv'Useless const omitted'"];
      my $code = 'sub ' . B::Deparse->new($deparseopts)->coderef2text($item);
      my @code = split(/\R/, $code);
      #
      # First item is no aligned
      #
      $self->_pushDesc('code', shift(@code));
      #
      # The first is aligned
      #
      if (@code) {
        $self->_pushLevel($reftype);
        map { $self->_pushLine; $self->_pushDesc('code', $_) } @code;
        $self->_popLevel($reftype)
      }
    } elsif ((! $reftype)
               ||
               (
                $reftype ne 'ARRAY'  &&
                $reftype ne 'HASH'   &&
                $reftype ne 'SCALAR' &&
                $reftype ne 'REF'
               )
              ) {
      #
      # Stringify if possible everything that we do not unfold
      #
      if (defined($item)) {
        my $string = eval { "$item" }; ## no critic qw/BuiltinFunctions::ProhibitStringyEval/
        if (defined($string)) {
          $self->_pushDesc($reftype eq 'REGEXP' ? 'regexp' :
                           $reftype eq 'CODE' ? 'code' :
                           'string', $string)
        } else {
          $self->_pushDesc('unknown', $self->unknown)
        }
      } else {
        $self->_pushDesc('undef', $self->undef)
      }
    }
    #
    # Show address ?
    #
    if ($refaddr && $self->show_address) {
      $self->_pushDesc('address_start', $self->address_start);
      $self->_pushDesc('address', $hex);
      $self->_pushDesc('address_end', $self->address_end)
    }
    #
    # Show blessed indication ?
    #
    $self->_pushDesc('blessed', $self->blessed) if $blessed
  }
  #
  # Eventually increase indice number
  #
  #
  # Prepare return value
  #
  my $rc;
  if (! $alreadyScanned) {
    if ($reftype) {
      if ($reftype eq 'ARRAY') {
        $rc = $item
      } elsif ($reftype eq 'HASH') {
        $rc = [ map { $_ => $item->{$_} } sort { ($a // '') cmp ($b // '') } keys %{$item} ]
      } elsif ($reftype eq 'SCALAR') {
        $rc = [ ${$item} ]
      } elsif ($reftype eq 'REF') {
        $rc = [ ${$item} ]
      }
    }
    if ($blessed && $self->methods) {
      $rc //= [];
      my $expanded = Class::Inspector->methods($blessed, 'expanded');
      if (defined($expanded) && reftype($expanded) eq 'ARRAY') {
        my @expanded = @{$expanded};
        my $class_idx = $[ + 1;
        my $method_idx = $class_idx + 1;
        my $ref_idx = $method_idx + 1;
        my %public_methods    = map { $_->[$method_idx] => $_->[$ref_idx] } grep { $_->[$method_idx] !~ /^\_/   } grep { $_->[$class_idx] eq $blessed } @expanded;
        my %private_methods   = map { $_->[$method_idx] => $_->[$ref_idx] } grep { $_->[$method_idx] =~ /^\_/   } grep { $_->[$class_idx] eq $blessed } @expanded;
        my %inherited_methods = map { $_->[$method_idx] => $_->[$ref_idx] }                                       grep { $_->[$class_idx] ne $blessed } @expanded;
        push(@{$rc}, {
                      public_methods     => \%public_methods,
                      private_methods    => \%private_methods,
                      inherited_methods  => \%inherited_methods
                     }
            )
      }
      if (Class::Inspector->loaded($blessed)) {
        push(@{$rc}, { loaded_filename => Class::Inspector->loaded_filename($blessed) })
      } else {
        push(@{$rc}, { resolved_filename => Class::Inspector->resolved_filename($blessed) })
      }
    }
  }

  $rc
}
#
# Internal methods
#
sub _concatenateLevels {
  my ($self, $level) = @_;
  my @levels = ();
  while ($level-- > 0) {
    unshift(@levels, $self->indice_start . ($self->_currentIndicePerLevel->[$level] - 1) . $self->indice_end)
  }
  return @levels
}

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

  if ($self->_canColor && $self->ansicolor) {
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

sub _canColor {
  my ($self) = @_;
  #
  # Mimic Data::Printer use of $ENV{ANSI_COLORS_DISABLED}
  #
  return 0 if exists($ENV{ANSI_COLORS_DISABLED});
  #
  # Add the support of ANSI_COLORS_ENABLED
  #
  return 1 if exists($ENV{ANSI_COLORS_ENABLED});
  #
  # that has precedence on the Windows check, returning 0 if we did not load Win32::Console::ANSI
  #
  return 0 if (is_os_type('Windows') && ! $_HAVE_Win32__Console__ANSI);
  return 1
}

with 'Data::Scan::Role::Consumer';

1;
