package OpenKore::Plugins::Simple;

use strict;

use base qw( Exporter );

use Plugins;
use Symbol;
use Time::HiRes qw( &time );
use Log qw( message warning error );

Plugins::register( 'simple', 'simple plugin framework', \&onUnload, \&onUnload );

my $hooks = Plugins::addHooks(    #
    [ Command_post => \&onCommandPost ],
    [ AI_pre       => \&onAIPre ],
);

our $plugins ||= {};
our $commands  = {};
our $queue = [];

rescan();

# Scan the plugins/simple directory and load any modules we find there.
sub rescan {

    # Can't put the scripts in plugins/simple because the normal Plugins scanner will find them.
    # Or we could use an extension other than .pl.
    #foreach my $dir ( map {"$_/simple"} @Settings::pluginsFolders ) {

    # For now, put scripts in control/all/simple/*.pl.
    foreach my $dir ( map {"$_/all/simple"} @Settings::controlFolders ) {
        next if !-d $dir;

        local *DIR;
        opendir DIR, $dir;
        my @files = grep {/\.pl$/} readdir DIR;
        closedir DIR;

        foreach my $file ( @files ) {
            my ( $name ) = $file =~ /^(.*)\.pl$/;
            Log::warning( "[simple] Loading plugin: $name\n" );
            my $fp;
            open $fp, "$dir/$file";
            local $/;
            my $code = <$fp>;
            close $fp;

            # Wrap it in a package and attempt to eval it.
            my $package = "OpenKore::Plugins::Simple::$name";
            $code = "package $package;use strict;use Globals;BEGIN { OpenKore::Plugins::Simple::DSL->import; };$code";
            eval $code;
            if ( $@ ) {
                Log::warning( "[simple] Unable to load plugin $name: $@\n" );
                next;
            }
            $plugins->{$name} = $package;

            # Collect a list of command aliases.
            no strict 'refs';
            my @aliases = sort grep {/^alias_/} keys %{"${package}::"};
            foreach ( @aliases ) {
                my ( $cmd ) = /^alias_(.*)$/;
                Log::warning( "[simple] [$name] Defining alias: $cmd\n" );
                $commands->{$cmd} = \&{"${package}::$_"};
            }
            use strict 'refs';
        }
    }
}

sub onUnload {
    Plugins::delHooks( $hooks );
    foreach ( sort keys %$plugins ) {
        Log::warning( "[simple] Unloading plugin: $_\n" );
        Symbol::delete_package( $plugins->{$_} );
    }
    $plugins = {};
}

sub onCommandPost {
    my ( undef, $args ) = @_;

    return if !$commands->{ $args->{switch} };
    $args->{return} = 1;

    exec_command( $commands->{ $args->{switch} }, [ Utils::parseArgs( $args->{input} ) ] );
}

sub onAIPre {
    return if AI::action ne 'simple';

    my $queue = AI::args->{queue};
    return AI::dequeue if !@$queue;

    my $cmd = AI::args->{current} = shift @$queue;
    if ( ref $cmd eq 'CODE' ) {
        $cmd = exec_command( $cmd );
        unshift @$queue, $cmd if $cmd;
    } else {
        Commands::run( $cmd );
    }
}

sub exec_command {
    my ( $sub, $args ) = @_;

    my $continuation;
    eval {
        $args ||= [];

        $queue = [];
        my @result = $sub->( @$args );
        $continuation = $sub  if @result and ref $result[0] eq 'HASH' && $result[0]->{continue};
        push @$queue, @result if !@$queue && @result;
        @$queue = grep { s/^\s+|\s+$//os if !ref $_;$_ } map { ref $_ ? $_ : split /\n/ } @$queue;

        # We got some commands! Put them on Kore's AI queue.
        if ( @$queue ) {
            AI::queue( simple => { queue => $queue } );
        }
    };
    if ( $@ ) {
        Log::warning( "[simple] Unable to execute command: $@\n" );
    }

    $continuation;
}

package OpenKore::Plugins::Simple::DSL;
# Provide a (hopefully simple to use) set of commands to make writing plugins easier.
#
# For example:
#   cmd 'move 100 100';
#   loop { 'move 100 100' } while2 { $config{moveme} };
#   while2 { $config{moveme} } loop { 'move 100 100' };
#   if2 { $config{moveme} } then { 'move 100 100' };
#
use Exporter qw(import);

BEGIN {
    our @EXPORT = qw( cmd loop while2 foreach2 if2 then );
};

sub cmd($@) {
    push @$OpenKore::Plugins::Simple::queue, @_;
}

sub loop(&;$) {
    if ( @_ > 1 ) {
        cmd $_[0];
        _while2( $_[1], $_[0] );
    } else {
        $_[0];
    }
}

sub while2(&;$) {
    if ( @_ > 1 ) {
        _while2( @_ );
    } else {
        $_[0];
    }
}

sub _while2 {
    my ( $test, $block ) = @_;
    cmd sub {
        return if !$test->();
        cmd $block;
        { continue => 1 };
    };
}

sub foreach2(&$) {
    my ( $values_sub, $block ) = @_;
    my $values;
    cmd sub {
        $values ||= [ $values_sub->() ];
        return if !@$values;
        my $arg = shift @$values;
        cmd sub { $_ = $arg;$block->() };
        { continue => 1 };
    };
}

sub if2(&$) {
    my ( $test, $block ) = @_;
    cmd sub { $block->() if $test->(); };
}

sub then(&) {
    $_[0];
}

package OpenKore::Plugins::Simple::Filter;
# Provide some source filters which make Simple plugins look more like Perl.

1;

__END__
Usage:

sub foo {
  'move 100 100',
  'move 110 110',
}

sub bar {'
  move 100 100
  move 110 110
'}
