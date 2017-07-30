package OpenKore::Plugin::Tour;

use strict;

use Plugins;
use Globals;
use Utils;
use Misc;
use Time::HiRes qw( &time );
use Log qw( message warning error );

our $config_defaults = {
    autoTour              => 0,
    autoTour_order        => 0,
    autoTour_start        => 0,
    autoTour_distFromGoal => 5,
};

Plugins::register( 'tour', 'move to a sequence of locations', \&onUnload, \&onUnload );

my $hooks = Plugins::addHooks(    #
    [ start3       => \&onStart3 ],
    [ AI_pre       => \&onAIPre ],
    [ Command_post => \&onCommandPost ],
    [ exp_gained   => \&onExpGained ],
);

###############################################################################
# Event Handlers

sub onExpGained {
    foreach ( reverse 0 .. $#ai_seq ) {
        next if $ai_seq[$_] ne 'tour';
        AI::args( $_ )->{xp} += $monsterBaseExp;
        AI::args( $_ )->{xpp} += $monsterBaseExp / ( $char->{exp_max} || 1_000_000_000 ) * 100;
        last;
    }
}

# Register config options, applying defaults.
sub onStart3 {
    foreach ( sort keys %$config_defaults ) {
        next if exists $config{$_};
        configModify( $_ => $config_defaults->{$_} );
    }
}

sub onUnload {
    Plugins::delHooks( $hooks );
    message "tour unloaded.\n";
}

sub onCommandPost {
    my ( undef, $args ) = @_;

    return if $args->{switch} ne 'tour';
    $args->{return} = 1;

    my $seq_str = ( split /\s+/, $args->{input}, 2 )[1];

    return onStart3() if $seq_str eq 'init';

	if ( $seq_str eq 'force' ) {
		foreach ( reverse 1 .. $#ai_seq ) {
			next if $ai_seq[$_] ne 'tour';
			unshift @ai_seq,      splice @ai_seq,      $_, 1;
			unshift @ai_seq_args, splice @ai_seq_args, $_, 1;
			Log::message( "[tour] Tour is now the top priority AI action.\n" );
			last;
		}
		return;
	}

    if ( $seq_str eq 'stop' ) {
        foreach ( reverse 0 .. $#ai_seq ) {
            next if $ai_seq[$_] ne 'tour';
            splice @ai_seq,      $_, 1;
            splice @ai_seq_args, $_, 1;
        }
        return;
    }

    if ( $seq_str eq 'pause' ) {
        my $i = AI::findAction( 'tour' );
        if ( $i eq '' ) {
            Log::error( "[tour] No tour in progress.\n" );
            return;
        }
        AI::args( $i )->{paused} = 1;
        Log::message( "[tour] Tour paused.\n" );
        Log::warning( "[tour] AI is on but no idle actions will be taken while the tour is paused.\n" );
        return;
    }

    if ( $seq_str eq 'resume' ) {
        my $i = AI::findAction( 'tour' );
        if ( $i eq '' ) {
            Log::error( "[tour] No tour in progress.\n" );
            return;
        }
        AI::args( $i )->{paused} = 0;
        Log::message( "[tour] Tour resumed.\n" );
        return;
    }

    if ( AI::findAction( 'tour' ) ne '' ) {
        Log::error( "[tour] Tour already in progress. Use 'tour stop' to cancel the current tour before starting a new one.\n" );
        return;
    }

    my $seq = OpenKore::Plugin::Tour::Sequence->new( { seq_str => $seq_str } );
    if ( !$seq->size ) {
        Log::error( "[tour] Invalid tour sequence.\n" );
        return;
    }

    # Reverse the sequence if requested.
    $seq->reverse if $config{autoTour_order} < 0;

    # Start at a different place if requested.
    $seq->rotate( $config{autoTour_start} ) if $config{autoTour_start} > 0;

    # Add the current map to the beginning if it's not already there.
	if ( !$seq->complete && $seq->map ne $field->baseName ) {
		$seq->{current} = { map => $field->baseName, pos => { x => $char->{pos_to}->{x}, y => $char->{pos_to}->{y} } };
		unshift @{ $seq->{seq} }, $seq->{current};
	}

    # The tour has less precedence than tactical actions.
    my $pos = 0;
    $pos++ while AI::action( $pos ) =~ /^(macro|attack|skill|take|gather|route|move|items_take|sitAuto)$/;

    splice @ai_seq, $pos, 0, 'tour';
    splice @ai_seq_args, $pos, 0, { seq => $seq, xp => 0, xpp => 0 };
 
    OpenKore::Plugin::Spread::send_log(
        {
            switch  => 'notice',
            message => "Tour [" . $seq->map . "] starting.",
        }
    );
}

sub run_tour {
    my ( $seq ) = @_;

    my $seq = OpenKore::Plugin::Tour::Sequence->new( { seq => $seq } );
    if ( !$seq->size ) {
        Log::error( "[tour] Invalid tour sequence.\n" );
        return;
    }

    # Reverse the sequence if requested.
    $seq->reverse if $config{autoTour_order} < 0;

    # Start at a different place if requested.
    $seq->rotate( $config{autoTour_start} ) if $config{autoTour_start} > 0;

    # The tour has less precedence than tactical actions.
    my $pos = 0;
    $pos++ while AI::action( $pos ) =~ /^(macro|attack|skill|take|gather|route|move|items_take)$/;

    splice @ai_seq, $pos, 0, 'tour';
    splice @ai_seq_args, $pos, 0, { seq => $seq, xp => 0, xpp => 0 };
}

sub onAIPre {
    return if AI::action ne 'tour';

    my $args = AI::args;
    my $seq  = $args->{seq};

    # Don't stand if the user forced us to sit.
    return if $ai_v{sitAuto_forcedBySitCommand};

    # Skip paused tour.
    return if $args->{paused};

    # Make sure we're on the correct map.
    if ( $seq->map ne $field->baseName ) {
        Log::warning( "[tour] Unexpectedly left map [" . $seq->map . "]. Aborting.\n" );
        AI::dequeue;
        return;
    }

    my $dist = distance( calcPosition( $char ), $seq->pos );

    # If the "current" position is visible and there's a command, perform it.
    if ( !$seq->complete && $seq->cmd eq 'openshop' && $dist <= 13 ) {
        $seq->{current}->{cmd} = '';
        my $pos = $seq->pos;
        foreach my $id ( @venderListsID ) {
            next if !$id;
            my $player = Actor::get( $id );
            next if $player->{pos_to}->{x} != $pos->{x};
            next if $player->{pos_to}->{y} != $pos->{y};
            AI::queue( openshop => { type => 'vend', id => $id, added => time, timeout => { timeout => 1 } } );
            return;
        }
    }

    # If close to the "current" position, go on to the next position.
    $seq->next if $dist <= 2 + $config{autoTour_distFromGoal} * 2;

    if ( $seq->complete ) {
        Log::message( "[tour] Tour complete. Have a nice day!\n" );
        OpenKore::Plugin::Spread::send_log(
            {
                switch  => 'notice',
                message => sprintf( 'Tour [%s] complete. Gained %d (%.1f%%) experience.', $seq->map, $args->{xp}, $args->{xpp} ),
            }
        );
        AI::dequeue;
        return;
    }

    main::ai_route(
        $seq->map, $seq->pos->{x}, $seq->pos->{y},
        attackOnRoute => 2,
        distFromGoal  => $config{autoTour_distFromGoal},
    );
    #Commands::run( "move $val->{pos}->{x} $val->{pos}->{y} $args->{map} 5" );
}

1;

package OpenKore::Plugin::Tour::Sequence;
###############################################################################
# A Sequence object, just so we can implement custom display in "ai print".

use strict;

sub new {
    my ( $class, $opt ) = @_;

    my $self = bless {}, $class;

    $self->{seq} = $self->parse_sequence( $opt->{seq_str} ) if $opt->{seq_str};
    $self->{seq} = $opt->{seq} if $opt->{seq};

    # Initialize.
    $self->_init;

    return $self;
}

sub _init {
    my ( $self, $steps ) = @_;
    return if !$self->{seq}->[0];
    $self->{step}    = 0;
    $self->{current} = { %{ $self->{seq}->[0] } };
}

sub current {
    my ( $self ) = @_;
    return $self->{current};
}

sub complete {
    my ( $self ) = @_;
    return 1 if $self->{step} >= $self->size;
    return 0;
}

sub next {
    my ( $self ) = @_;
    $self->{step}++ if !$self->complete;
    return if $self->complete;
    my $step = $self->{seq}->[ $self->{step} ];
    $self->{current}->{$_} = $step->{$_} foreach keys %$step;
    return $self->current;
}

sub map {
    my ( $self ) = @_;
    return $self->current->{map};
}

sub pos {
    my ( $self ) = @_;
    return $self->current->{pos};
}

sub cmd {
    my ( $self ) = @_;
    return $self->current->{cmd} || '';
}

sub size {
    my ( $self ) = @_;
    return scalar @{ $self->{seq} };
}

sub reverse {
    my ( $self ) = @_;
    @{ $self->{seq} } = reverse @{ $self->{seq} };
    return $self->{seq};
}

sub rotate {
    my ( $self, $steps ) = @_;
    $steps = int( $steps ) % $self->size;
    if ( $steps ) {
        push @{ $self->{seq} }, splice @{ $self->{seq} }, 0, $steps;
    }
    $self->_init;
}

sub dumpArgs {
    my ( $self ) = @_;
    my $i = -1;
    return join ' ', $self->map, map { $i++;my $hilight = $i == $self->{step} ? '::' : '';sprintf '%s%d,%d%s', $hilight, $_->{pos}->{x}, $_->{pos}->{y}, $hilight; } @{ $self->{seq} };
}

###############################################################################
# Utility Functions

# Format: Comma-separated list of maps and positions. Must start with a map.
# Example: ice_dun01,157 30,136 33,118 32,82 21,42 26
sub parse_sequence {
    my ( $self, $seq_str ) = @_;

    $seq_str =~ s/^\s+|\s+$//gos;

    my $map = '';
    my $seq = [];
    foreach ( split /\s*,\s*/, $seq_str ) {
        if ( /^\w+$/o ) {
            $map = $_;
        } elsif ( /^(?:(\w+) )?(\d+)\s+(\d+)$/o ) {
            push @$seq, { map => $map, pos => { x => $2, y => $3 }, cmd => $1 };
        } else {
            return [];
        }
    }

    return $seq;
}

1;
