{ # inlined until part of Devel::Declare
package
  My::Devel::Declare::Context::Simple;

use Devel::Declare ();
use Scope::Guard;
use strict;
use warnings;

sub new {
  my $class = shift;
  bless {@_}, $class;
}

sub init {
  my $self = shift;
  @{$self}{ qw(Declarator Offset) } = @_;
  $self;
}

sub offset : lvalue { shift->{Offset}; }
sub declarator { shift->{Declarator} }

sub skip_declarator {
  my $self = shift;
  $self->offset += Devel::Declare::toke_move_past_token( $self->offset );
}

sub skipspace {
  my $self = shift;
  $self->offset += Devel::Declare::toke_skipspace( $self->offset );
}

sub strip_name {
  my $self = shift;
  $self->skipspace;
  if (my $len = Devel::Declare::toke_scan_word( $self->offset, 1 )) {
    my $linestr = Devel::Declare::get_linestr();
    my $name = substr( $linestr, $self->offset, $len );
    substr( $linestr, $self->offset, $len ) = '';
    Devel::Declare::set_linestr($linestr);
    return $name;
  }
  return;
}

sub strip_proto {
  my $self = shift;
  $self->skipspace;

  my $linestr = Devel::Declare::get_linestr();
  if (substr( $linestr, $self->offset, 1 ) eq '(') {
    my $length = Devel::Declare::toke_scan_str( $self->offset );
    my $proto  = Devel::Declare::get_lex_stuff();
    Devel::Declare::clear_lex_stuff();
    $linestr = Devel::Declare::get_linestr();
    substr( $linestr, $self->offset, $length ) = '';
    Devel::Declare::set_linestr($linestr);
    return $proto;
  }
  return;
}

sub get_curstash_name {
  return Devel::Declare::get_curstash_name;
}

sub shadow {
  my $self  = shift;
  my $pack = $self->get_curstash_name;
  Devel::Declare::shadow_sub( $pack . '::' . $self->declarator, $_[0] );
}

sub inject_if_block {
  my $self    = shift;
  my $inject = shift;
  $self->skipspace;
  my $linestr = Devel::Declare::get_linestr;
  if (substr( $linestr, $self->offset, 1 ) eq '{') {
    substr( $linestr, $self->offset + 1, 0 ) = $inject;
    Devel::Declare::set_linestr($linestr);
  }
}

sub scope_injector_call {
  return ' BEGIN { ' . __PACKAGE__ . '::inject_scope }; ';
}

sub inject_scope {
  my $self = shift;
  $^H |= 0x120000;
  $^H{DD_METHODHANDLERS} = Scope::Guard->new(sub {
      my $linestr = Devel::Declare::get_linestr;
      my $offset  = Devel::Declare::get_linestr_offset;
      substr( $linestr, $offset, 0 ) = ';';
      Devel::Declare::set_linestr($linestr);
  });
}

1;

package
  My::Devel::Declare::MethodInstaller::Simple;

use base 'My::Devel::Declare::Context::Simple';

use Devel::Declare ();
use Sub::Name;
use strict;
use warnings;

sub install_methodhandler {
  my $class = shift;
  my %args  = @_;
  {
    no strict 'refs';
    *{$args{into}.'::'.$args{name}}   = sub (&) {};
  }

  my $ctx = $class->new(%args);
  Devel::Declare->setup_for(
    $args{into},
    { $args{name} => { const => sub { $ctx->parser(@_) } } }
  );
}

sub parser {
  my $self = shift;
  $self->init(@_);

  $self->skip_declarator;
  my $name   = $self->strip_name;
  my $proto  = $self->strip_proto;
  my @decl   = $self->parse_proto($proto);
  my $inject = $self->inject_parsed_proto(@decl);
  if (defined $name) {
    $inject = $self->scope_injector_call() . $inject;
  }
  $self->inject_if_block($inject);
  if (defined $name) {
    my $pkg = $self->get_curstash_name;
    $name = join( '::', $pkg, $name )
      unless( $name =~ /::/ );
    $self->shadow( sub (&) {
      my $code = shift;
      # So caller() gets the subroutine name
      no strict 'refs';
      *{$name} = subname $name => $code;
    });
  } else {
    $self->shadow(sub (&) { shift });
  }
}

sub parse_proto { }

sub inject_parsed_proto {
  return $_[1];
}

1;
}

package CGI::Application::Plugin::RunmodeDeclare;

use warnings;
use strict;

our $VERSION = '0.03';

use base 'My::Devel::Declare::MethodInstaller::Simple';
use Data::Alias ();
use Carp qw(croak);

sub import {
    my $class = shift;
    my $caller = caller;

    my %remap = (
            runmode   => runmode   =>
            startmode => startmode =>
            errormode => errormode =>
            invocant  => '$self' =>
            @_ );

    $class->install_methodhandler(
        into         => $caller,
        name         => $remap{runmode},
        pre_install  => \&_setup_runmode,
        invocant     => $remap{invocant},
    );
    $class->install_methodhandler(
        into         => $caller,
        name         => $remap{startmode},
        pre_install  => \&_setup_startmode,
        invocant     => $remap{invocant},
    );
    $class->install_methodhandler(
        into         => $caller,
        name         => $remap{errormode},
        pre_install  => \&_setup_errormode,
        invocant     => $remap{invocant},
    );
}


my %REGISTRY;
# per-macro setup
sub _split {
    my $n = shift; my ($p,$l) = $n =~ /(.*)::(.*)/; return ($p, $l);
}
sub _setup_runmode {
    my ($fullname, $code) = @_;
    my ($pkg, $name) = _split($fullname);
    $pkg->add_callback( init => sub { $_[0]->run_modes([ $name ]) } );
}
sub _setup_startmode {
    my ($fullname, $code) = @_;
    no strict 'refs'; no warnings 'uninitialized';
    my ($pkg, $name) = _split($fullname);
    croak "start mode redefined (from $REGISTRY{$pkg}{start_mode_installed})" if $REGISTRY{$pkg}{start_mode_installed};
    $pkg->add_callback( init => sub { $_[0]->run_modes([ $name ]); $_[0]->start_mode($name); } );
    $REGISTRY{$pkg}{start_mode_installed} = $fullname;
}
sub _setup_errormode {
    my ($fullname, $code) = @_;
    no strict 'refs'; no warnings 'uninitialized';
    my ($pkg, $name) = _split($fullname);
    croak "error mode redefined (from $REGISTRY{$pkg}{error_mode_installed})" if $REGISTRY{$pkg}{error_mode_installed};
    $pkg->add_callback( init => sub { $_[0]->error_mode($name); } );
    $REGISTRY{$pkg}{error_mode_installed} = $fullname;
}

sub strip_name {
    my $ctx = shift;

    my $name = $ctx->SUPER::strip_name;
    $ctx->{pre_install}->($ctx->get_curstash_name . '::' . $name);

    return $name;
}

sub parse_proto {
    my $ctx = shift;
    my ($proto) = @_;
    $proto ||= '';

    # Do all the signature parsing here
    my %signature;
    $signature{invocant} = $ctx->{invocant};
    $signature{invocant} = $1 if $proto =~ s{^(\$.+):\s*}{};

    my @protos = split /\s*,\s*/, $proto;
    for my $idx (0..$#protos) {
        my $sig = $signature{$idx} = {};
        my $proto = $protos[$idx];

#            print STDERR "proto: $proto\n";

        $sig->{proto}               = $proto;
        $sig->{idx}                 = $idx;
        $sig->{is_at_underscore}    = $proto eq '@_';
        $sig->{is_ref_alias}        = $proto =~ s{^\\}{}x;

        $sig->{trait}   = $1 if $proto =~ s{ \s+ is \s+ (\S+) \s* }{}x;
        $sig->{default} = $1 if $proto =~ s{ \s* = \s* (.*) }{}x;

        my($sigil, $name) = $proto =~ m{^ (.)(.*) }x;
        $sig->{is_optional} = ($name =~ s{\?$}{} or $sig->{default});
        $sig->{sigil}       = $sigil;
        $sig->{name}        = $name;
    }

    # XXX At this point we could do sanity checks

    return \%signature;
}

# Turn the parsed signature into Perl code
sub inject_parsed_proto {
    my $ctx = shift;
    my $signature = shift;

    my @code;
    push @code, "my $signature->{invocant} = shift;";

    for( my $idx = 0; my $sig = $signature->{$idx}; $idx++ ) {
        next if $sig->{is_at_underscore};

        my $sigil = $sig->{sigil};
        my $name  = $sig->{name};

        # These are the defaults.
        my $lhs = "my ${sigil}${name}";
        my $rhs = (!$sig->{is_ref_alias} and $sig->{sigil} =~ /^[@%]$/) ? "\@_[$idx..\$#_]" : "\$_[$idx]";

        # Handle a default value
        $rhs = "\@_ > $idx ? $rhs : $sig->{default}" if defined $sig->{default};

        # XXX We don't do anything with traits right now

        # XXX is_optional is ignored

        # params; app first, then query
        $rhs .= "; ${sigil}${name} = $signature->{invocant}->param('${name}') unless defined ${sigil}${name}";
        $rhs .= "; ${sigil}${name} = $signature->{invocant}->query->param('${name}') unless defined ${sigil}${name}";

        # Handle \@foo
        if( $sig->{is_ref_alias} ) {
            push @code, sprintf 'Data::Alias::alias(%s = %s);', $lhs, $sigil."{$rhs}";
        }
        else {
            push @code, "$lhs = $rhs;";
        }
    }

    # All on one line.
    return join ' ', @code;
}


1; # End of CGI::Application::Plugin::RunmodeDeclare


__END__

=head1 NAME

CGI::Application::Plugin::RunmodeDeclare - Declare runmodes with keywords

=head1 VERSION

Version 0.03

=head1 SYNOPSIS

    package My::CgiApp;

    use base 'CGI::Application';
    use CGI::Application::Plugin::RunmodeDeclare;

    startmode hello { "Hello!" }

    runmode world($name) {
        return $self->hello
        . ', '
        . $name || "World!";
    }

    errormode oops($c: $exception) {
        return "Something went wrong at "
        . $c->get_current_runmode
        . ". Exception: $exception";
    }

=head1 DESCRIPTION

This module allows you to declare run modes with a simple keyword. It's heavily
inspired by L<Method::Signatures>, and copies all of its features (from
version 0.10 at least).

It respects inheritance: run modes defined in the superclass are also available
in the subclass.

Beyond automatically registering the run mode, and providing C<$self>, it also
optionally pulls named parameters from C<< $self->query->param >> or
C<< $self->param >>.

=over 4

=item * Basic example

    runmode foo { $self->bar }

This declares the run mode "foo". Notice how C<$self> is ready for use.

=item * Rename invocant

    runmode bar ($c:) { $c->baz }

Same as above, only use C<$c> instead of C<$self>.

    use CGI::Application::Plugin::RunmodeDeclare invocant => '$c';
    runmode baz { $c->quux }

Same as above, but every runmode gets C<$c> by default. You can still say C<runmode ($self:)>
to rename the invocant.

=item * With a parameter list

    runmode baz ( $id, $name ) {
        return $self->wibble("I received $id and $name from a form submission
                              or a method invocation.");
    }

Here, we specify that the method expects two parameters, C<$id> and C<$name>.
Values can be supplied through a method call (e.g. C<< $self->baz(1, "me") >>),
or from the query object (e.g. from C</script?id=42;name=me>), or from this cgiapp
object (e.g. C<< $self->param( id => 42 ) >>).

=back

=head1 EXPORT

=over 4

=item * errormode

Define the run mode that serves as C<< $self->error_mode >>. You can only declare one
C<errormode> per package.

=item * startmode

Define the run mode that serves as C<< $self->start_mode >>. You can only declare one
C<startmode> per package.

=item * runmode

Define run mode.

=back

=head1 AUTHOR

Rhesa Rozendaal, C<< <rhesa at cpan.org> >>

=head1 DIAGNOSTICS

=over 4

=item * error mode redefined (from %s) at %s line %s

You tried to install another errormode. Placeholders are filled with

 * fully qualified name of existing errormode
 * file name
 * line number

=item * start mode redefined (from %s) at %s line %s

You tried to install another startmode. Placeholders are filled with

 * fully qualified name of existing startmode
 * file name
 * line number

=back

=head1 BUGS

Please report any bugs or feature requests to C<bug-cgi-application-plugin-runmodedeclare at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=CGI-Application-Plugin-RunmodeDeclare>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc CGI::Application::Plugin::RunmodeDeclare


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=CGI-Application-Plugin-RunmodeDeclare>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/CGI-Application-Plugin-RunmodeDeclare>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/CGI-Application-Plugin-RunmodeDeclare>

=item * Search CPAN

L<http://search.cpan.org/dist/CGI-Application-Plugin-RunmodeDeclare>

=back


=head1 ACKNOWLEDGEMENTS

Matt S. Trout for L<Devel::Declare>, and Michael G. Schwern for providing
the inspiration with L<Method::Signatures>.

=head1 COPYRIGHT & LICENSE

Copyright 2008 Rhesa Rozendaal, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.


=cut

