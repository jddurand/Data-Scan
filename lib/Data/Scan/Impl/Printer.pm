use strict;
use warnings FATAL => 'all';

package Data::Scan::Impl::Printer;

# ABSTRACT: Data::Scan printer implementation

# VERSION

# AUTHORITY

=head1 DESCRIPTION

Data::Scan::Impl::Printer is an example of an implementation of the Data::Scan::Role::Consumer role. This implementation is sort of Data::Printer alternative.

=head1 SYNOPSIS

    use strict;
    use warnings FATAL => 'all';
    use Data::Scan;
    use Data::Scan::Impl::Printer;

    my $this = bless([ 'var1', 'var2', {'a' => 'b', 'c' => 'd'}, \undef, \\undef, [], sub { return 'something' } ], 'TEST');
    my $consumer = Data::Scan::Impl::Printer->new(with_deparse => 1);
    Data::Scan->new(consumer => $consumer)->process($this);
    $consumer->dsprint;

=cut

use B::Deparse;
use Class::Inspector;
use Moo;
use MooX::HandlesVia;
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
my $_ASCII_PRINT = quotemeta(join('', map { chr } (32,33..126)));
my $_NON_ASCII_PRINT_RE = qr/[^$_ASCII_PRINT]/;

=head1 CONSTRUCTOR OPTIONS

Here the list of supported options, every name is preceded by its type.

=cut

=head2 FileHandle handle

Handle for for dsprint(). Default is \*STDOUT.

=cut

has handle            => (is => 'ro', isa => FileHandle,       default => sub { return \*STDOUT  });

=head2 Str indent

Indentation. Default is '  '.

=cut

has indent            => (is => 'ro', isa => Str,              default => sub { return '  '      });

=head2 PositiveOrZeroInt max_depth

Maximum unfold level. Default is 0, meaning no maximum.

=cut

has max_depth         => (is => 'ro', isa => PositiveOrZeroInt, default => sub { return 0        });

=head2 Str undef

Representation of an undefined value. Default is 'undef'.

=cut

has undef             => (is => 'ro', isa => Str,              default => sub { return 'undef'   });

=head2 Str unknown

Representation of an unknown value. Default is '???'.

=cut

has unknown           => (is => 'ro', isa => Str,              default => sub { return '???'     });

=head2 Str newline

Separator between lines. Default is "\n".

=cut

has newline           => (is => 'ro', isa => Str,              default => sub { return "\n"      });

=head2 Bool with_ansicolor

Use ANSI colors. Default is a false value if $ENV{ANSI_COLORS_DISABLED} exists, else a true value if $ENV{ANSI_COLORS_ENABLED} exists, else a false value if L<Win32::Console::ANSI> cannot be loaded and you are on Windows, else a true value.

=cut

has with_ansicolor    => (is => 'ro', isa => Bool,             default => sub { return __PACKAGE__->_canColor });

=head2 Str array_start

Representation of the start of an array. Default is '['.

=cut

has array_start       => (is => 'ro', isa => Str,              default => sub { return '['      });

=head2 Str array_next

Representation of separator between array elements. Default is ','.

=cut

has array_next        => (is => 'ro', isa => Str,              default => sub { return ','       });

=head2 Str array_end

Representation of the end of an array. Default is ']'.

=cut

has array_end         => (is => 'ro', isa => Str,              default => sub { return ']'       });

=head2 Str hash_start

Representation of the start of a hash. Default is '{'.

=cut

has hash_start        => (is => 'ro', isa => Str,              default => sub { return ' {'      });

=head2 Str hash_next

Representation of separator between hash elements, where an element is the tuple {key,value}. Default is ','.

=cut

has hash_next         => (is => 'ro', isa => Str,              default => sub { return ','       });

=head2 Str hash_end

Representation of the end of a hash. Default is '}'.

=cut

has hash_end          => (is => 'ro', isa => Str,              default => sub { return '}'       });

=head2 Str hash_separator

Representation of hash separator between a key and a value. Default is '=>'.

=cut

has hash_separator    => (is => 'ro', isa => Str,              default => sub { return ' => '    });

=head2 Str indice_start

Representation of internal indice count start. Default is '['.

=cut

has indice_start          => (is => 'ro', isa => Str,              default => sub { return '['       });

=head2 Str indice_end

Representation of internal indice count end. Default is ']'.

=cut

has indice_end          => (is => 'ro', isa => Str,              default => sub { return '] '      });

=head2 Bool with_indices_full

Use full internal indice representation, i.e. show indices from the top level up to current level, as if the tree would have been only composed of array references to array references, and so on. Default is a true value.

=cut

has with_indices_full => (is => 'ro', isa => Bool,             default => sub { return !!1       });

=head2 Str address_start

Representation of the start of an address. Default is '('.

=cut

has address_start     => (is => 'ro', isa => Str,              default => sub { return '('       });

=head2 Str address_format

Format of an address. Default is '0x%x'.

=cut

has address_format    => (is => 'ro', isa => Str,              default => sub { return '0x%x'    });

=head2 Str address_end

Representation of the end of an address. Default is ')'.

=cut

has address_end       => (is => 'ro', isa => Str,              default => sub { return ')'       });

=head2 Str ref_start

Representation of the start of a reference. Default is '\'.

=cut

has ref_start         => (is => 'ro', isa => Str,              default => sub { return '\\'      });

=head2 Str ref_end

Representation of the end of a reference. Default is the empty string.

=cut

has ref_end           => (is => 'ro', isa => Str,              default => sub { return ''        });

=head2 Bool with_address

Show address of any reference. Default is a false value.

=cut

has with_address      => (is => 'ro', isa => Bool,             default => sub { return !!0       });

=head2 Bool with_array_indice

Show array indices. Default is a true value.

=cut

has with_array_indice => (is => 'ro', isa => Bool,             default => sub { return !!1       });

=head2 Bool with_hash_indice

Show hash indices. Default is a true value.

=cut

has with_hash_indice  => (is => 'ro', isa => Bool,             default => sub { return !!1       });

=head2 Bool with_deparse

Show deparsed subroutine references. Default is a false value.

=cut

has with_deparse      => (is => 'ro', isa => Bool,             default => sub { return !!0       });

=head2 Bool with_methods

Show public, private and inherited methods. Default is a true value.

=cut

has with_methods      => (is => 'ro', isa => Bool,             default => sub { return !!1       });

=head2 Bool with_filename

Show loaded or resolved filename. Default is a false value.

=cut

has with_filename     => (is => 'ro', isa => Bool,             default => sub { return !!0       });

=head2 HashRef[Str] colors

Explicit ANSI color per functionality. The absence of a color definition means the corresponding value will be printed as-is. A color is defined following the Term::ANSIColor specification, as a string.

Supported keys of this hash and their eventual default setup is:

=over

=item string => undef

Generic stringified value.

=item blessed => 'bold'

Blessed name.

=item regexp => undef

Stringified regexp.

=item array_start => 'blue'

Array start.

=item array_next => 'blue'

Separator between array elements.

=item array_end => 'blue'

Array end.

=item hash_start => 'blue'

Hash start.

=item hash_separator => 'blue'

Separator between hash key and value.

=item hash_next => 'blue'

Separator between a hash value and the next hash key.

=item hash_end => 'blue'

Hash end.

=item ref_start => undef

Reference start.

=item ref_end => undef

Reference end.

=item indice_full => 'magenta'

Full indice.

=item indice_start => 'magenta'

Indice start.

=item indice_value => 'magenta'

Indice value.

=item indice_end => 'magenta'

Indice end.

=item undef => 'red'

The undefined value.

=item unknown => 'bold red'

An unknown value.

=item address_start => 'magenta'

Address start.

=item address_value => 'magenta'

Address value.

=item address_end => 'magenta'

Address end.

=item code => 'yellow'

Deparsed or stringified code reference.

=item already_scanned => 'green'

Already scanned reference. Such item will always be represented using "var[...]", where [...] is the full indice representation.

=back

=cut

has colors            => (is => 'ro', isa => HashRef[Str|Undef], default => sub {
                            return {
                                    blessed         => 'bold',
                                    string          => undef,
                                    regexp          => undef,

                                    array_start     => 'blue',
                                    array_next      => 'blue',
                                    array_end       => 'blue',

                                    hash_start      => 'blue',
                                    hash_separator  => 'blue',
                                    hash_next       => 'blue',
                                    hash_end        => 'blue',

                                    ref_start       => undef,
                                    ref_end         => undef,

                                    indice_full     => 'magenta',
                                    indice_start    => 'magenta',
                                    indice_value    => 'magenta',
                                    indice_end      => 'magenta',

                                    undef           => 'red',
                                    unknown         => 'bold red',
                                    address_start   => 'magenta',
                                    address_value   => 'magenta',
                                    address_end     => 'magenta',
                                    code            => 'yellow',
                                    already_scanned => 'green'
                                   }
                          },
                         handles_via => 'Hash',
                         handles => {
                                     _keys_colors => 'keys',
                                     _exists_colors => 'exists',
                                     _get_colors => 'get'
                                    });
#
# Internal attributes
#
has _lines                  => (is => 'rw', isa => ArrayRef,
                               handles_via => 'Array',
                               handles => {
                                           _set_lines => 'set',
                                           _get_lines => 'get',
                                           _push_lines => 'push',
                                           _elements_lines => 'elements'
                                          }
                               );
has _currentLevel           => (is => 'rw', isa => PositiveOrZeroInt,
                               handles_via => 'Number',
                               handles => {
                                           _add_currentLevel => 'add',
                                           _sub_currentLevel => 'sub'
                                          }
                               );
has _currentIndicePerLevel  => (is => 'rw', isa => ArrayRef[PositiveOrZeroInt],
                               handles_via => 'Array',
                               handles => {
                                           _get_currentIndicePerLevel => 'get',
                                           _push_currentIndicePerLevel => 'push',
                                           _pop_currentIndicePerLevel => 'pop',
                                           _set_currentIndicePerLevel => 'set',
                                          }
                               );
has _currentReftypePerLevel => (is => 'rw', isa => ArrayRef[Str],
                               handles_via => 'Array',
                               handles => {
                                           _get_currentReftypePerLevel => 'get',
                                           _push_currentReftypePerLevel => 'push',
                                           _pop_currentReftypePerLevel => 'pop'
                                          }
                               );
has _seen                   => (is => 'rw', isa => HashRef[PositiveOrZeroInt],
                                handles_via => 'Hash',
                                handles => {
                                            _exists_seen => 'exists',
                                            _get_seen => 'get',
                                            _set_seen => 'set',
                                           }
                               );
has _indice_start_nospace   => (is => 'rw', isa => Str);  # C.f. BUILD
has _indice_end_nospace     => (is => 'rw', isa => Str);
has _colors_cache           => (is => 'rw', isa => HashRef[Str|Undef],
                               handles_via => 'Hash',
                               handles => {
                                           _exists_colors_cache => 'exists',
                                           _get_colors_cache => 'get',
                                           _set_colors_cache => 'set'
                                          }
                               );
has _concatenatedLevels     => (is => 'rw', isa => ArrayRef[Str],
                               handles_via => 'Array',
                               handles => {
                                           _get_concatenatedLevels => 'get',
                                           _push_concatenatedLevels => 'push',
                                           _pop_concatenatedLevels => 'pop'
                                          }
                               );

#
# Required methods
#

=head1 SUBROUTINES/METHODS

=head2 dsstart

Will be called when scanning is starting. It is resetting all internal attributes used to keep the context.

=cut

sub dsstart  {
  my ($self) = @_;

  $self->_lines(['']);
  $self->_currentLevel(0);
  $self->_currentIndicePerLevel([]);
  $self->_currentReftypePerLevel([]);
  $self->_seen({});
  $self->_concatenatedLevels([]);

  my $indice_start_nospace = $self->indice_start;
  my $indice_end_nospace = $self->indice_end;
  $indice_start_nospace =~ s/\s//g;
  $indice_end_nospace =~ s/\s//g;
  $self->_indice_start_nospace($indice_start_nospace);
  $self->_indice_end_nospace($indice_end_nospace);
  #
  # Precompute color attributes
  #
  $self->_colors_cache({});
  if ($self->with_ansicolor) {
    foreach ($self->_keys_colors) {
      my $color = $self->_get_colors($_);
      if (defined($color)) {
        my $colored = colored('dummy', $color);
        #
        # ANSI color spec is clear: attributes before the string, followed by
        # the string, followed by "\e[0m". We do not support the eventual
        # $EACHLINE hack.
        #
        if ($colored =~ /(.+)dummy\e\[0m$/) {
          $self->_set_colors_cache($_, substr($colored, $-[1], $+[1] - $-[1]))
        } else {
          $self->_set_colors_cache($_, undef)
        }
      } else {
        $self->_set_colors_cache($_, undef)
      }
    }
  } else {
    foreach ($self->_keys_colors) {
      $self->_set_colors_cache($_, undef)
    }
  }

  return
}

=head2 dsend

Will be called when scanning is ending. Returns a true value.

=cut

sub dsend { return !!1 }

=head2 dsprint

Print current output to $self->handle. If $self->handle is blessed and can do the 'print' method, this will be used. Otherwise, $self->handle will be considered eligible for the native perl's print command. Returns a string.

=cut

sub dsprint {
  my ($self, $handle) = @_;

  $handle //= $self->handle;
  my $string = join($self->newline, $self->_elements_lines);
  if (Scalar::Util::blessed($handle) && $handle->can('print')) {
    return $handle->print($string)
  } else {
    return print $handle $string
  }
}

=head2 dsopen

Called when an unfolded content is opened.

=cut

sub dsopen {
  my ($self, $item) = @_;

  my $reftype = reftype $item;
  my $blessed = Scalar::Util::blessed $item;

  if    ($reftype eq 'ARRAY') { $self->_pushDesc('array_start', $self->array_start) }
  elsif ($reftype eq 'HASH')  { $self->_pushDesc('hash_start',  $self->hash_start)  }
  else                        { $self->_pushDesc('ref_start',   $self->ref_start)   }

  #
  # Precompute the string describing previous level.
  #
  if ($self->with_indices_full) {
    #
    # Here $self->_currentLevel is the value before we increase it
    #
    if (my $currentLevel = $self->_currentLevel) {
      $self->_push_concatenatedLevels($self->_get_concatenatedLevels(-1) .
                                      $self->_indice_start_nospace .
                                      $currentLevel .
                                      $self->_indice_end_nospace)
    } else {
      $self->_push_concatenatedLevels('');
    }
  }

  $self->_pushLevel($reftype);

  return
}

=head2 dsclose

Called when an unfolded content is closed.

=cut

sub dsclose {
  my ($self, $item) = @_;

  #
  # Remove precomputed string describing this level.
  #
  $self->_pop_concatenatedLevels if ($self->with_indices_full);

  $self->_popLevel;

  my $reftype = reftype $item;
  if    ($reftype eq 'ARRAY') { $self->_pushLine; $self->_pushDesc('array_end', $self->array_end) }
  elsif ($reftype eq 'HASH')  { $self->_pushLine; $self->_pushDesc('hash_end', $self->hash_end)   }
  else                        {                   $self->_pushDesc('ref_end',   $self->ref_end)   }

  return
}

=head2 dsread

Called when an unfolded content is read. Returns eventual unfolded content.

=cut

sub dsread {
  my ($self, $item) = @_;

  my $refaddr = refaddr($item);
  my $blessed = Scalar::Util::blessed($item) // '';
  my $reftype = reftype($item) // '';
  #
  # Precompute things that always have the same value
  #
  my $indice_start                   = $self->indice_start;
  my $indice_end                     = $self->indice_end;
  my $indice_start_nospace           = $self->_indice_start_nospace;
  my $indice_end_nospace             = $self->_indice_end_nospace;
  #
  # Push a newline or a '=>' and prefix with indice if in a fold
  #
  my $currentLevel = $self->_currentLevel;
  if ($currentLevel) {
    my $currentReftypePerLevel = $self->_get_currentReftypePerLevel(-1);
    my $currentIndicePerLevel = $self->_get_currentIndicePerLevel(-1);
    if ($currentReftypePerLevel eq 'ARRAY' or $currentReftypePerLevel eq 'HASH') {
      my $show_indice;
      if ($currentReftypePerLevel eq 'ARRAY') {
        $self->_pushDesc('array_next', $self->array_next) if ($currentIndicePerLevel > $[);
        $self->_pushLine;
        $show_indice = $self->with_array_indice
      } else {
        if ($currentIndicePerLevel % 2) {
          $self->_pushDesc('hash_separator', $self->hash_separator);
        } else {
          $self->_pushDesc('hash_next', $self->hash_next) if ($currentIndicePerLevel > 0);
          $self->_pushLine;
        }
        $show_indice = $self->with_hash_indice
      }
      if ($show_indice) {
        if ($self->with_indices_full) {
          #
          # We know that $self->_concatenatedLevels is an ArrayRef.
          # $currentLevel is a true value, this mean there is at least
          # one element in $self->_concatenatedLevels.
          #
          $self->_pushDesc('indice_full', $self->_get_concatenatedLevels(-1) . $indice_start_nospace . $currentIndicePerLevel . $indice_end_nospace)
        } else {
          $self->_pushDesc('indice_start', $indice_start);
          $self->_pushDesc('indice_value', $currentIndicePerLevel);
          $self->_pushDesc('indice_end', $indice_end)
        }
      }
    }
    $self->_set_currentIndicePerLevel(-1, $self->_get_currentIndicePerLevel(-1) + 1)
  }
  #
  # See how this can be displayed
  #
  my $alreadyScanned;
  if ($refaddr) {
    if ($self->_exists_seen($refaddr)) {
      $alreadyScanned = $self->_get_seen($refaddr);
      #
      # Already scanned !
      #
      $self->_pushDesc('already_scanned', $alreadyScanned)
    } else {
      #
      # Determine the "location" in terms of an hypothetical "@var" describing the tree
      # Note that we already increased $currentLevel
      #
      my $var = 'var';
      $var .= $self->_get_concatenatedLevels(-1) . $indice_start_nospace . ($self->_get_currentIndicePerLevel(-1) - 1) . $indice_end_nospace if ($currentLevel);
      $self->_set_seen($refaddr, $var)
    }
  }
  if (! $alreadyScanned) {
    if ($blessed && $reftype ne 'REGEXP') {
      #
      # A regexp appears as being blessed in perl.
      # Priority is given to blessed name except if it is a regexp.
      #
      $self->_pushDesc('blessed', $blessed)
    } elsif ($reftype eq 'CODE' && $self->with_deparse) {
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
        $self->_popLevel
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
    if ($refaddr && $self->with_address) {
      my $address_format = $self->address_format;
      $self->_pushDesc('address_start', $self->address_start);
      $self->_pushDesc('address_value', length($address_format) ? sprintf($address_format, $refaddr) : $refaddr);
      $self->_pushDesc('address_end', $self->address_end)
    }
  }
  #
  # Eventually increase indice number
  #
  #
  # Prepare return value
  #
  my $rc;
  #
  # Max depth option value ?
  #
  my $max_depth = $self->max_depth;
  return $rc if ($max_depth && $currentLevel >= $max_depth);
  #
  # Unfold if not already done and if this can be unfolded
  #
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
    if ($blessed && $self->with_methods) {
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
      if ($self->with_filename) {
        if (Class::Inspector->loaded($blessed)) {
          push(@{$rc}, { filename => Class::Inspector->loaded_filename($blessed) })
        } else {
          push(@{$rc}, { filename => Class::Inspector->resolved_filename($blessed) })
        }
      }
    }
  }

  $rc
}
#
# Internal methods
#
sub _pushLevel {
  my ($self, $reftype) = @_;

  $self->_push_currentReftypePerLevel($reftype);
  $self->_push_currentIndicePerLevel($reftype eq 'ARRAY' ? $[ : 0);       # $[ only used for ARRAY
  $self->_currentLevel($self->_add_currentLevel(1));
  return
}

sub _popLevel {
  my ($self) = @_;

  $self->_pop_currentReftypePerLevel;
  $self->_pop_currentIndicePerLevel;
  $self->_currentLevel($self->_sub_currentLevel(1));
  return
}

sub _pushLine {
  my ($self) = @_;

  $self->_push_lines($self->indent x $self->_currentLevel);
  return
}

sub _pushDesc {
  my ($self, $what, $desc) = @_;

  if ($what eq 'string' && ! looks_like_number($desc)) {
    #
    # Detect any non ANSI character and enclose result within ""
    #
    $desc =~ s/$_NON_ASCII_PRINT_RE/sprintf('\\x{%x}', ord(${^MATCH}))/egpo;
    $desc = '"' . $desc . '"'
  }
  #
  # We know that _colors_cache is a HashRef, and that _lines is an ArrayRef
  #
  my $color_cache = $self->_exists_colors_cache($what) ? $self->_get_colors_cache($what) : undef;
  $desc = $color_cache . $desc . "\e[0m" if (defined($color_cache));
  $self->_set_lines(-1, $self->_get_lines(-1) . $desc);

  return
}

sub _canColor {
  my ($class) = @_;
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

=head1 NOTES

If with_methods option is on, L<Class::Inspector> (and not L<Package::Stash> like what does L<Data::Printer>) is used to get public, private and other (labelled inherited, then) methods. Thus, notion of methods, usage of @ISA etc, could look different to what L<Data::Printer> say.

=head1 SEE ALSO

L<Class::Inspector>, L<Data::Scan::Impl::Printer>, L<Term::ANSIColor>, L<Win32::Console::ANSI>

=cut

with 'Data::Scan::Role::Consumer';

1;
