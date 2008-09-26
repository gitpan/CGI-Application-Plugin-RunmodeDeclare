package  # hide from cpan
My::DDCS;


# Stolen from Devel::Declare's t/method-no-semi.t
use Devel::Declare ();
use Scope::Guard;
use Sub::Name;
use strict;
use warnings;

sub install_keyword {
    my $class = shift;
    my $ctx   = $class->new;
    my %args  = @_;
    $ctx->{install_cb} = $args{pre_install} || sub {};
    # I don't really understand why we need to declare method
    # in the caller's namespace.
    {
        no strict 'refs';
        *{$args{into}.'::'.$args{name}}   = sub (&) {};
    }
    Devel::Declare->setup_for(
        $args{into},
        { $args{name} => { const => $ctx->mk_parser },
        },
    );

}

sub new {
    my $class = shift;
    bless {
        Declarator => undef,
        Offset     => undef,
    }, $class;
}

sub skip_declarator {
    my $ctx = shift;
    $ctx->{Offset} += Devel::Declare::toke_move_past_token($ctx->{Offset});
}

sub skipspace {
    my $ctx = shift;
    $ctx->{Offset} += Devel::Declare::toke_skipspace($ctx->{Offset});
}

sub strip_name {
    my $ctx = shift;
    $ctx->skipspace;
    if (my $len = Devel::Declare::toke_scan_word($ctx->{Offset}, 1)) {
        my $linestr = Devel::Declare::get_linestr();
        my $name = substr($linestr, $ctx->{Offset}, $len);
        substr($linestr, $ctx->{Offset}, $len) = '';
        Devel::Declare::set_linestr($linestr);
        return $name;
    }
    return;
}

sub strip_proto {
    my $ctx = shift;
    $ctx->skipspace;

    my $linestr = Devel::Declare::get_linestr();
    if (substr($linestr, $ctx->{Offset}, 1) eq '(') {
        my $length = Devel::Declare::toke_scan_str($ctx->{Offset});
        my $proto = Devel::Declare::get_lex_stuff();
        Devel::Declare::clear_lex_stuff();
        $linestr = Devel::Declare::get_linestr();
        substr($linestr, $ctx->{Offset}, $length) = '';
        Devel::Declare::set_linestr($linestr);
        return $proto;
    }
    return;
}

sub get_curstash_name {
    return Devel::Declare::get_curstash_name;
}

sub shadow {
    my $ctx = shift;
    my $pack = $ctx->get_curstash_name;
    Devel::Declare::shadow_sub("${pack}::$ctx->{Declarator}", $_[0]);
}

sub inject_if_block {
    my $ctx = shift;
    my $inject = shift;
    $ctx->skipspace;
    my $linestr = Devel::Declare::get_linestr;
    if (substr($linestr, $ctx->{Offset}, 1) eq '{') {
        substr($linestr, $ctx->{Offset}+1, 0) = $inject;
        Devel::Declare::set_linestr($linestr);
    }
}

sub scope_injector_call {
    my $ctx = shift;
    return ' BEGIN { ' . __PACKAGE__ . '::inject_scope }; ';
}

sub parse_proto { }
sub inject_parsed_proto { }
sub mk_parser {
    my $ctx = shift;

    return sub {
    @{$ctx}{qw(Declarator Offset)} = @_;
    $ctx->skip_declarator;
    my $name = $ctx->strip_name;
    my $proto = $ctx->strip_proto;
    my @decl = $ctx->parse_proto($proto);
    my $inject = $ctx->inject_parsed_proto(@decl);
    if (defined $name) {
        $inject = $ctx->scope_injector_call().$inject;
    }
    $ctx->inject_if_block($inject);
    if (defined $name) {
        my $pkg = $ctx->get_curstash_name;
        $name = join('::', $pkg, $name)
          unless ($name =~ /::/);
    }
    $ctx->shadow(sub (&) {
        no strict 'refs';
        my $code = shift;
        # So caller() gets the subroutine name
        *{$name} = subname $name => $code if defined $name;
    });
    };
}

sub inject_scope {
    my $ctx = shift;
    $^H |= 0x120000;
    $^H{DD_METHODHANDLERS} = Scope::Guard->new(sub {
        my $linestr = Devel::Declare::get_linestr;
        my $offset = Devel::Declare::get_linestr_offset;
        substr($linestr, $offset, 0) = ';';
        Devel::Declare::set_linestr($linestr);
    });
}

1; # End of My::DDCS

package CGI::Application::Plugin::RunmodeDeclare;

use warnings;
use strict;

our $VERSION = '0.02';

use base qw/My::DDCS/;
use Data::Alias ();
use Carp qw(croak);

sub import {
    my $class = shift;
    my $caller = caller;

    my %remap = (
            runmode   => runmode   =>
            startmode => startmode =>
            errormode => errormode =>
            @_ );

    $class->install_keyword(
        into         => $caller,
        name         => $remap{runmode},
        pre_install  => \&_setup_runmode,
    );
    $class->install_keyword(
        into         => $caller,
        name         => $remap{startmode},
        pre_install  => \&_setup_startmode,
    );
    $class->install_keyword(
        into         => $caller,
        name         => $remap{errormode},
        pre_install  => \&_setup_errormode,
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
    croak "error mode redefined (from $REGISTRY{$pkg}{error_mode_installed})" if $REGISTRY{$pkg}{error_mode_installed};
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
    $ctx->{install_cb}->($ctx->get_curstash_name . '::' . $name);

    return $name;
}

sub parse_proto {
    my $ctx = shift;
    my ($proto) = @_;
    $proto ||= '';

    # Do all the signature parsing here
    my %signature;
    $signature{invocant} = '$self';
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

        # query params
        $rhs .= "; ${sigil}${name} = $signature->{invocant}->query->param('${name}') unless defined ${sigil}${name}";
        $rhs .= "; ${sigil}${name} = $signature->{invocant}->param('${name}') unless defined ${sigil}${name}";
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

Version 0.02

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

