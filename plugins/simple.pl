package OpenKore::Plugins::Simple;
###############################################################################
# Provide support for very simple plugins, removing all (or most) of the
# scaffolding necessary to turn a set of functions into a plugin.
#
# Plugins intended for use with this plugin should use the ".spl" extension
# instead of ".pl".
#
# Conventions:
#
# * The plugin name may be provided by setting the package global $name: our $name = 'myplugin';
# ** If no plugin name is provided, the filename will be used.
#
# * The plugin description may be provided by setting the package global $desc: our $desc = 'a simple plugin';
# ** If no plugin description is provided, it will default to "no description provided".
#
# * All methods starting with "hook_" are auto-registered as hooks.
# ** To include a slash in a hook, use "_slash_". For example: "hook_packet_slash_deal_begin" is registered as "packet/deal_begin".
# ** To include a colon in a hook, use "_colon_". For example: "hook_AI_colon_colon_queue" is registered as "AI::queue".
#
# * All methods starting with "cmd_" are auto-registered as commands.
# ** Sub-commands may be registered using a second underscore. For example, "cmd_foo_bar" can be called via "foo bar" on the console.
#
# * The "hook_unload" method (if defined) will be called when the plugin is unloaded.
# * The "hook_reload" method (if defined) will be called when the plugin is reloaded. If there is no "reload" method, the "hook_unload" method (if defined) will be called instead.

use strict;
use warnings;
no warnings 'redefine';

use Settings qw( %sys );
use Log qw();
use Misc qw( center );
use Plugins qw();
use Translation qw( T TF );
use Utils qw( swrite );

our $extension = 'spl';
our $tag       = '[simple] ';
our $commands ||= [];
our $plugins  ||= [];

Plugins::register( 'simple_plugins', 'simple plugin framework', \&hook_unload, \&hook_reload );

# Tag all output with our plugin name.
sub message { Log::message( $tag . shift( @_ ), @_ ); }
sub warning { Log::warning( $tag . shift( @_ ), @_ ); }
sub error { Log::error( $tag . shift( @_ ), @_ ); }

sub hook_load {
    $commands = Commands::register( @{ detect_commands( __PACKAGE__ ) } );
    register_files();
}

sub hook_unload {
    Commands::unregister( $commands );
    cmd_simple_unload( $_->{name} ) foreach @$plugins;
}

sub hook_reload {
    Commands::unregister( $commands );
}

# DONE: Call register_files.
# DONE: Register self using self. Actually using some features, but not the full registration process.
# DONE: Re-implement "plugin" command for simple plugins?
# DONE: unLoad
# DONE: Move Plugins::Symbols here, somehow. (Use "import" instead of "use"?)
# TODO: Document namespace pollution from Plugins::Symbols.

sub register_files {
    my $condition;
    if ( !exists $sys{loadPlugins} ) {
        message T( "Loading all plugins (by default)...\n", 'plugins' );
        $condition = \&Plugins::c_loadAll;
    } elsif ( !$sys{loadPlugins} ) {
        message T( "Automatic loading of plugins disabled\n", 'plugins' );
        return;
    } elsif ( $sys{loadPlugins} eq '1' ) {
        message T( "Loading all plugins...\n", 'plugins' );
        $condition = \&Plugins::c_loadAll;
    } elsif ( $sys{loadPlugins} eq '2' ) {
        message T( "Selectively loading plugins...\n", 'plugins' );
        $condition = \&Plugins::c_loadSelected;
    } elsif ( $sys{loadPlugins} eq '3' ) {
        message T( "Selectively skipping plugins...\n", 'plugins' );
        $condition = \&Plugins::c_loadNotSelected;
    }

    my $files = find_files( $condition );
    foreach my $file ( sort @$files ) {
        if ( grep { $_->{file} eq $file } @$plugins ) {
            message( TF( "Loading plugin %s... already loaded, skipping.\n", $file ), 'plugins' );
            next;
        }
        message( TF( "Loading plugin %s...\n", $file ), 'plugins' );
        register_file( $file );
    }
}

sub find_files {
    my ( $condition ) = @_;
    $condition ||= \&Plugins::c_loadAll;
    [ map {"$_->{dir}/$_->{name}$_->{ext}"} grep { $condition->( $_->{name} ) } Plugins::getFilesFromDirs( [ Settings::getPluginsFolders ], $extension, 'cvs', 1 ) ];
}

sub register_file {
    my ( $file )    = @_;
    my ( $package ) = $file =~ m{([^/]+)\.[^.]+$};
    $package =~ s/\W/_/gs;
    my $fp;
    if ( !open $fp, '<', $file ) {
        error sprintf "Plugin [$file] error: %s\n", $!;
        return;
    }
    local $/;
    my $file_contents = <$fp>;
    close $fp;
    my $r = eval qq{
		package OpenKore::Plugins::$package;
		use strict;
		use warnings;
		BEGIN { OpenKore::Plugins::Simple::Symbols->import; };
		no warnings 'redefine';
# Magic comment so eval reports the right file and line for syntax errors. Must be at the start of the line!
# line 1 "$file"
		$file_contents;
		1;
	};
    if ( $r ) {
        OpenKore::Plugins::Simple::register_package( "OpenKore::Plugins::$package", $file );
    } else {
        if ( $@ ) {
            error sprintf "Plugin [$file] contains syntax errors:\n%s", $@;
        } else {
            error sprintf "Plugin [$file] error: %s\n", $!;
        }
    }
}

##
# void register_package(String package)
# package: A package name (eg, OpenKore::Plugins::ABCPlugin::Part1).
# Returns: 1 if the plugin has been successfully registered, 0 if a plugin with the same name is already registered.
sub register_package {
    my ( $package, $file ) = @_;

    my $unload = defined &{"${package}::hook_unload"} ? \&{"${package}::hook_unload"} : undef;
    my $reload = defined &{"${package}::hook_reload"} ? \&{"${package}::hook_reload"} : $unload;

    # TODO: Register is tightly bound to this method. Merge them?
    register(
        {
            file        => $file,
            package     => $package,
            name        => detect_name( $package ),
            description => detect_description( $package ),
            hooks       => Plugins::addHooks( @{ detect_hooks( $package ) } ),
            commands    => Commands::register( @{ detect_commands( $package ) } ),
            unload      => $unload,
            reload      => $reload,
        }
    );
}

sub register {
    my ( $plugin ) = @_;
    push @$plugins, $plugin;
}

our $cmd_simple_desc = 'Simple plugin system.';

our $cmd_simple_list_desc = 'List loaded plugins.';
sub cmd_simple_list {
    my $msg = center( T( " Currently loaded plugins " ), 79, '-' ) . "\n";
    $msg .= T( "Name                 Description\n" );
    $msg .= swrite( '@<<<<<<<<<<<<<<<<<<< @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<', [ $_->{name}, $_->{description} ] ) foreach sort { $a->{name} cmp $b->{name} } @$plugins;
    $msg .= ( '-' x 79 ) . "\n";
    Log::message $msg, 'list';
}

our $cmd_simple_load_args = '<name|all>';
our $cmd_simple_load_desc = 'Load a plugin.';
sub cmd_simple_load {
    my ( $name ) = @_;

    return Commands::run( 'help simple' ) if !$name;

    # "simple load all"
    return register_files() if $name eq 'all';

    $name = "$name.$extension" if $name !~ /\.$extension$/;

    # Find the plugin file. Allow partial paths.
    my ( $file ) = grep {/(^|\/)\Q$name\E$/} @{ find_files() };
    if ( !$file ) {
        warning TF( "File %s does not exist.\n", $name );
        return;
    }

    # Make sure this plugin isn't already loaded.
    if ( grep { $_->{file} eq $file } @$plugins ) {
        warning TF( "Plugin %s is already loaded.\n", $file );
        return;
    }

    message( TF( "Loading plugin %s...\n", $file ), 'plugins' );
    register_file( $file );
}

# TODO: Verify that this works without any hooks and/or commands.
our $cmd_simple_unload_args = '<name|all>';
our $cmd_simple_unload_desc = 'Unload a plugin.';
sub cmd_simple_unload {
    my ( $name ) = @_;

    return Commands::run( 'help simple' ) if !$name;

    # "simple unload all"
    if ( $name eq 'all' ) {
        cmd_simple_unload( $_->{name} ) foreach @$plugins;
        return;
    }

    my @todo = grep { $_->{name} eq $name } @$plugins;
    if ( !@todo ) {
        error TF( "Plugin %s not found.\n", $name ), 'plugins';
        return;
    }

    @$plugins = grep { $_->{name} ne $name } @$plugins;
    foreach ( @todo ) {
        message TF( "Unloading plugin %s...\n", $_->{name} ), 'plugins';
        $_->{unload}->() if $_->{unload};
        Plugins::delHooks( $_->{hooks} );
        Commands::unregister( $_->{commands} );
    }
    @$plugins = grep { $_->{name} ne $name } @$plugins;
}

our $cmd_simple_reload_args = '<name>';
our $cmd_simple_reload_desc = 'Reload a plugin.';
sub cmd_simple_reload {
    my ( $name ) = @_;

    return Commands::run( 'help simple' ) if !$name;

    my @todo = grep { $_->{name} eq $name } @$plugins;
    if ( !@todo ) {
        error TF( "Plugin %s not found.\n", $name ), 'plugins';
        return;
    }

    @$plugins = grep { $_->{name} ne $name } @$plugins;
    foreach ( @todo ) {
        message TF( "Reloading plugin %s...\n", $_->{name} ), 'plugins';
        $_->{reload}->() if $_->{reload};
        Plugins::delHooks( $_->{hooks} );
        Commands::unregister( $_->{commands} );
        register_file( $_->{file} );
    }
}

##
# String detect_name(String package_name)
# package: A package name (eg, OpenKore::Plugins::ABCPlugin::Part1).
# Returns: Detected plugin name, either the $desc global from the package or a lowercase version of the package name.
#
# If auto-converted from the package name, the output will always match the regular expression /^\w+$/.
# For example, 'OpenKore::Plugins::ABCPlugin::Part1' will become 'abc_plugin_part_1'.
sub detect_name {
    my ( $package ) = @_;

    no strict 'refs';
    my $name = ${"${package}::name"};
    use strict 'refs';

    if ( !$name ) {
        $name = $package;
        $name =~ s/^((openkore|plugins?)::)*//i;
        $name =~ s/\W+/_/g;
        $name =~ s/([A-Z])([A-Z]*)([A-Z])/$1.lc($2).$3/eg;
        $name =~ s/([A-Z])/'_'.lc($1)/eg;
        $name =~ s/(\d+)/_${1}_/g;
        $name =~ s/__/_/g;
        $name =~ s/^_|_$//g;
    }

    $name;
}

sub detect_global {
    my ( $package, $name ) = @_;

    no strict 'refs';
    ${"${package}::${name}"};
}

sub detect_description {
    my ( $package ) = @_;

    detect_global( $package, 'desc' ) || 'no description provided';
}

sub detect_hooks {
    my ( $package ) = @_;
    my @methods = grep {/^hook_/} @{ list_package_methods( $package ) };
    my $hooks = [];
    foreach my $method ( @methods ) {
        my $hook_sub   = \&{"${package}::$method"};
        my $sub        = sub { $hook_sub->( $_[1] ); };
        my $hook_names = method_name_to_hook_names( $method );
        push @$hooks, [ $_ => $sub ] foreach @$hook_names;
    }
    $hooks;
}

sub detect_commands {
    my ( $package, $plugin_name ) = @_;
    $plugin_name ||= detect_name( $package );
    my @methods = grep {/^cmd_/} @{ list_package_methods( $package ) };
    my $map = {};
    foreach my $method ( @methods ) {
        next if !defined &{"${package}::$method"};
        my $cmd_sub = \&{"${package}::$method"};
        my ( undef, $cmd, $subcmd ) = split '_', $method, 3;
        $map->{$cmd}->{ $subcmd || '' } = {
            name        => $subcmd || '',
            args        => detect_global( $package, "${method}_args" ) || '',
            description => detect_global( $package, "${method}_desc" ) || 'no description provided',
            callback    => $cmd_sub,
        };
    }
    [ map { [ $_, [($map->{$_}->{''} || {description => detect_global($package, "cmd_${_}_desc") || "$_ command from the $plugin_name plugin"})->{description}, map { ["$_->{name} $_->{args}",$_->{description}] } sort { $a->{name} cmp $b->{name} } values %{$map->{$_}}], sub { dispatch_command( $map, $_[0], Utils::parseArgs( $_[1] ) ) } ] } keys %$map ];
}

sub list_package_methods {
    my ( $package ) = @_;

    no strict 'refs';
    my $methods = [ sort keys %{"${package}::"} ];
    use strict 'refs';

    $methods;
}

sub dispatch_command {
    my $cmds = shift;
    my $cmd  = shift;
    if ( @_ && $cmds->{$cmd}->{ $_[0] } ) {
        my $subcmd = shift;
        $cmds->{$cmd}->{$subcmd}->{callback}->( @_ );
    } elsif ( $cmds->{$cmd}->{''} ) {
        $cmds->{$cmd}->{''}->{callback}->( @_ );
    } else {
        error "Command [$cmd] does not accept parameters [@_].\n";
        error "Known sub-commands for this command are:\n";
        error "  $cmd $_\n" foreach sort keys %{ $cmds->{$cmd} };
    }
}

sub method_name_to_hook_names {
    my ( $name ) = @_;
    my $hooks = [];
    $name =~ s/^hook_//;
    push @$hooks, $name;
    my $token_map = { slash => '/', colon => ':' };
    my @tokens = split '_', $name;
    @tokens = map { $token_map->{$_} || $_ } @tokens;
    $name = join '_', @tokens;
    $name =~ s/_?(\W)_?/$1/g;
    push @$hooks, $name if $name ne $hooks->[0];
    $hooks;
}

# Do this at the very end, so globals (eg, descriptions) are already defined.
hook_load();

1;

package OpenKore::Plugins::Simple::Symbols;

use strict;
use base 'Exporter';

# Import generally useful symbols from various other packages.
# This results in massive namespace pollution, but also a big improvement to ease of use.
use Globals;
use Misc;
use Utils;
use Log qw( debug message warning error );
use Time::HiRes qw( time sleep );

# Export all symbols to users of this package.
BEGIN {
    our @EXPORT = do {
        no strict 'refs';
        sort map {
            my @syms;
            push @syms, "\$$_" if *{ __PACKAGE__ . "::$_" }{SCALAR};
            push @syms, "\@$_" if *{ __PACKAGE__ . "::$_" }{ARRAY};
            push @syms, "\%$_" if *{ __PACKAGE__ . "::$_" }{HASH};
            push @syms, "\&$_" if *{ __PACKAGE__ . "::$_" }{CODE};
            @syms;
        } grep {/[a-z]/} keys %{ __PACKAGE__ . '::' };
    };
};

1;
