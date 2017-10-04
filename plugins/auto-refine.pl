#########################################################################
#  OpenKore - Auto-Refine
#  Copyright (c) 2008 Bibian
#
# This plugin is licensed under Creative Commons "Attribution-NonCommercial-ShareAlike 2.5"
#
# You are free:
#    * to copy, distribute, display, and perform the work
#    * to make derivative works
# 
# Under the following conditions:
#    * by Attribution: You must attribute the work in the manner specified by the author or licensor.
#    * Noncommercial: You may not use this work for commercial purposes.
#    * Share Alike: If you alter, transform, or build upon this work, you may distribute the resulting work only under a license identical to this one.
#
#    * For any reuse or distribution, you must make clear to others the license terms of this work.
#    * Any of these conditions can be waived if you get permission from the copyright holder.
#
# Your fair use and other rights are in no way affected by the above.
#
# This is a human-readable summary of the Legal Code ( Full License: http://creativecommons.org/licenses/by-nc-sa/2.5/legalcode ). 
# Disclaimer: http://creativecommons.org/licenses/disclaimer-popup?lang=en
# 
#########################################################################
package OpenKore::Plugin::AutoRefine;

use strict;

use Plugins;
use Settings;
use Log qw(message error);
use Utils;
use Globals;
use Task;
use Task::MapRoute;
use Task::TalkNPC;
use Time::HiRes qw( &time );

Plugins::register('autorefine', 'auto upgrade weapon/armor', \&unload);
my $hooks = Plugins::addHooks(
    [ 'AI_pre'                 => \&main ],
    [ 'Command_post'           => \&onCommandPost ],
    [ 'packet_skilluse'        => \&onSkillUse ],
    [ 'packet/equip_item'      => \&onEquipItem ],
    [ 'packet/unequip_item'    => \&onUnequipItem ],
    [ 'packet/upgrade_message' => \&onUpgradeMessage ],
    [ 'packet/unit_levelup'    => \&onUnitLevelup ],
);

my ($todo,$routeCallback,$talkCallback,$startRefine,$talking);

sub unload {
    Plugins::delHooks($hooks);
}

# No need to go any faster than 1 iteration per second
$timeout{autorefine}{timeout} ||= 3.5;
$timeout{autorefine}{time} ||= time;

$timeout{autorefine}{timeout} = 0.8;

sub onCommandPost {
    my ( undef, $args ) = @_;

    return if $args->{input} !~ /^autorefine\b/o;
    $args->{return} = 1;

    my ( $cmd, $subcmd ) = $args->{input} =~ /^(autorefine)\s*(.*?)\s*$/o;

    if ( $subcmd eq 'select' ) {
        selectItem();
        if ( $config{refine_todo} ) {
            message "First item to refine is: $config{refine_todo}: $todo->{item}->{name}\n"
        } else {
            message "Nothing to refine!\n"
        }
    }

    if ( $subcmd eq 'cartselect' ) {
        selectItem(0, 1);
        if ( $config{refine_todo} ) {
            message "First cart item to refine is: $config{refine_todo}"
        } else {
            message "Nothing to refine!\n"
        }
    }

    if ( $subcmd eq 'list' ) {
        selectItem(1);
        if ( $config{refine_todo} ) {
            message "Items to refine are: $config{refine_todo}"
        } else {
            message "Nothing to refine!\n"
        }
    }

    if ( $subcmd eq 'cartlist' ) {
        selectItem(1, 1);
        if ( $config{refine_todo} ) {
            message "Cart items to refine are: $config{refine_todo}"
        } else {
            message "Nothing to refine!\n"
        }
    }
}

sub main {
    return if !$net || $net->getState != Network::IN_GAME;
    return if $char->{sitting};
    return if !exists $config{'autoRefine_0'};

    if ( $AI == AI::AUTO && !AI::inQueue( 'refine_plugin' ) ) {
        return if !timeOut( $timeout{autorefine} );

        if ( !selectItem() ) {
            $timeout{autorefine}{time} = time;
            return;
        }

        message "Starting refine for $todo->{item}->{name}\n";
        $todo->{upgrade} = $todo->{item}->{upgrade};
        transition( $todo, 'start' );
        AI::queue( refine_plugin => $todo );
        return;
    }

    return if AI::action ne 'refine_plugin';

    $todo = AI::args;

    return if $todo->{timeout} && !timeOut( $todo->{timeout} );

    event( 'timeout' );

    # If we didn't get an expected event, throw an error.
    if ( !$todo->{event} ) {
        $todo->{error} = "Did not see any expected event [@{[sort keys %{$todo->{want_events}}]}] within the timeout [$todo->{timeout}->{timeout}].";
        transition( $todo, 'error' );
        return;
    }

    # At this point, we must be in an expected state, with state and event both defined.

    my $old_state = $todo->{state};
    if ( $todo->{useSkill} ) {
        refine_using_skill( $todo );
    } else {
        refine_using_npc( $todo );
    }
    return if $todo->{state} ne $old_state;

    if ( $todo->{state} eq 'error' ) {
        Log::error( "[refine] $todo->{error} Aborting." );
        transition( $todo, 'end' );
        return;
    }

    if ( $todo->{state} eq 'end' ) {
        checkItem( $todo );
        AI::dequeue;
        $timeout{autorefine}{time} = time;
        return;
    }

    $todo->{error} = "In unexpected state [$todo->{state}].";
    transition( $todo, 'error' );
    return;
}

# start --> waiting_for_skill --> waiting_to_refine --> waiting_for_result --> end
sub refine_using_skill {
    my ( $todo ) = @_;

    if ( $todo->{state} eq 'start' ) {
        my $skill = Skill->new( auto => $todo->{useSkill} );
        my $skillTask = Task::UseSkill->new(
            actor     => $char,
            target    => $char,
            actorList => $playersList,
            skill     => $skill,
            priority  => Task::USER_PRIORITY,
        );
        my $task = new Task::ErrorReport( task => $skillTask );
        $taskManager->add( $task );
        transition( $todo, 'waiting_for_skill', 6, ['skill_use'] );
        return;
    }

    if ( $todo->{state} eq 'waiting_for_skill' ) {
        transition( $todo, 'waiting_to_refine', 0.2 );
        return;
    }

    if ( $todo->{state} eq 'waiting_to_refine' ) {
        $messageSender->sendWeaponRefine( $todo->{item}->{index} );
        transition( $todo, 'waiting_for_result', 6, ['unit_levelup'] );
        return;
    }

    if ( $todo->{state} eq 'waiting_for_result' ) {
        transition( $todo, 'end' );
        return;
    }
}

# start --> moving --> equipping? --> talking --> end
sub refine_using_npc {
    my ( $todo ) = @_;

    # Check location and move if necessary.
    if ( $todo->{state} eq 'start' ) {
        my @items = items_equipped();
        if ( @items ) {
            $todo->{error} = "Too many items [" . @items . "] equipped. Cannot use npc refine.";
            transition( $todo, 'error' );
            return;
        }
        transition( $todo, 'moving' );
        route( $todo );
        return;
    }

    if ( $todo->{state} eq 'moving' ) {
        if ( $todo->{item}->{equipped} ) {
            transition( $todo, 'equipping' );
        } else {
            $todo->{item}->equip;
            transition( $todo, 'equipping', 6, ['equip_item'] );
        }
        return;
    }

    if ( $todo->{state} eq 'equipping' ) {
        transition( $todo, 'talking' );
        talkNPC( $todo );
        return;
    }

    if ( $todo->{state} eq 'talking' ) {
        transition( $todo, 'unequipping' );
        return;
    }

    if ( $todo->{state} eq 'unequipping' ) {
        my $item = $char->inventory->getByServerIndex( $todo->{item}->{index} );
        if ( $item ) {
            $item->unequip;
            transition( $todo, 'end', 6, ['unequip_item'] );
        } else {
            transition( $todo, 'end' );
        }
        return;
    }
}

sub items_equipped {

    # Get a list of refinable slots.
    my @slots = grep { !/^(midHead|lowHead|costume.*|leftAccessory|rightAccessory)$/ } @Actor::Item::slots;
    my @items = grep { $_ } map { $char->{equipment}->{$_} } @slots;
Log::message("Items: ".join(', ', map { $_->{name} } @items)."\n");
    @items;
}

sub can_npc {
    my @items = items_equipped();
    @items < 2;
}

sub strip_commas {
	my ( $str ) = @_;
	$str =~ s/,//gos;
	$str;
}

sub selectItem {
    my ( $find_all, $find_in_cart ) = @_;

    $todo = undef;

    my @eligible;
    for ( my $i = 0 ; exists $config{"autoRefine_$i"} ; $i++ ) {
        my $prefix = "autoRefine_$i";
        my $skillName = $config{"${prefix}_useSkill"};
        my $skill = $skillName ? Skill->new( auto => $skillName ) : undef;
        my ( $x, $y, $map );
        next if $config{"${prefix}_disabled"};
        next if !$config{$prefix};
        next if !$config{"${prefix}_refineStone"};
		if ( $skill ) {
			next if !$char->{skills}->{ $skill->getHandle };
			next if $char->{sp} < $char->{skills}->{ $skill->getHandle }->{sp};
		} else {
			next if !$config{"${prefix}_npc"};
			next if !$config{"${prefix}_npcSequence"};
			next if strip_commas( $config{"${prefix}_zeny"} ) > $char->{zeny};
			next if $config{"${prefix}_npc"} !~ /^\s*(\d+)\s+(\d+)\s+(\S+)/;
            ( $x, $y, $map ) = ( $1, $2, $3 );
			next if $config{"${prefix}_npc"} && items_equipped();
		}

        my @item_names = split /\s*,\s*/, lc $config{$prefix};

        my @eligible2;
        my $list = $find_in_cart ? $char->cart->getItems : $char->inventory->getItems;
        foreach my $item ( @$list ) {
            next if !$config{"${prefix}_useSkill"} && !$item->{identified};
            next if !defined $item->{upgrade};
            next if $item->{upgrade} < ($config{"${prefix}_minRefine"} || 0);
            next if $item->{upgrade} >= ($config{"${prefix}_maxRefine"} || 10);
            my $name = lc $item->name;
            $name =~ s/\+\d+\s+//;
            next if !grep { $name eq $_ } @item_names;
            my $metal = $char->inventory->getByName( $config{"${prefix}_refineStone"} );
            next if !$metal;
            push @eligible2,
              {
                prefix   => $prefix,
                item     => $item,
                metal    => $metal,
                useSkill => $config{"${prefix}_useSkill"},
                x        => $x,
                y        => $y,
                map      => $map,
                npc_id   => $config{"${prefix}_npcId"},
                sequence => $config{"${prefix}_npcSequence"},
                depth    => $config{"${prefix}_depthFirst"},
              };
        }
        if ( $config{"${prefix}_keep"} ) {
            @eligible2 = reverse sort {
                $a->{item}->{upgrade} <=> $b->{item}->{upgrade}
                    || $b->{item}->{equipped} <=> $a->{item}->{equipped}
                    || $a->{item}->{index} <=> $b->{item}->{index}
            } @eligible2;
            splice @eligible2, 0, $config{"${prefix}_keep"};
        }
        push @eligible, @eligible2;
    }

    $config{refine_todo} = undef;
    $config{refine_todo_item} = undef;
    return if !@eligible;

    my @eligible_depth = sort {
           $b->{item}->{upgrade} <=> $a->{item}->{upgrade}
        || $b->{item}->{equipped} <=> $a->{item}->{equipped}
        || $a->{item}->{index} <=> $b->{item}->{index}
    } grep { $_->{depth} } @eligible;
    my @eligible_breadth = sort {
           $a->{item}->{upgrade} <=> $b->{item}->{upgrade}
        || $b->{item}->{equipped} <=> $a->{item}->{equipped}
        || $a->{item}->{index} <=> $b->{item}->{index}
    } grep { !$_->{depth} } @eligible;
    @eligible = ( @eligible_depth, @eligible_breadth );
    $todo = $eligible[0];

    $config{refine_todo} = $todo->{prefix};
    $config{refine_todo_item} = $todo->{item} && $todo->{item}->{name};

    if ( $find_all ) {
        $config{refine_todo} = join ',', map { $_->{prefix} } @eligible;
        $config{refine_todo_item} = join ',', map { $_->{item}->{name} } @eligible;
    }

    return $config{refine_todo};
}

sub route {
    my ( $todo ) = @_;

    my $target_dist = 14;

    my $dist = distance( calcPosition( $char ), $todo );

    return if $field->baseName eq $todo->{map} && $dist <= 14;

    $char->route( $todo->{map}, $todo->{x}, $todo->{y}, distFromGoal => $target_dist );
}

sub talkNPC {
    my ( $todo ) = @_;

    my $talkTask = Task::TalkNPC->new(
        x        => $todo->{x},
        y        => $todo->{y},
        nameID   => $todo->{npc_id},
        sequence => $todo->{sequence},
    );
    AI::queue( 'NPC' => $talkTask );
}

sub checkItem {
    my ( $todo ) = @_;

    my $item = $char->inventory->getByServerIndex( $todo->{item}->{index} );

    my $result = '';
    if ( $item && $item->{upgrade} > $todo->{upgrade} ) {
        message "Upgraded $todo->{item}->{name} to +$item->{upgrade}!\n";
        $result = 'success';
    } elsif ( $item && $item->{upgrade} < $todo->{upgrade} ) {
        message "Upgrade failed, and refine rate was reduced.\n";
        $result = 'downgrade';
    } elsif ( $item && $item->{upgrade} == $todo->{upgrade} ) {
        message "Upgrade failed, but item was not destroyed.\n";
        $result = 'unchanged';
    } else {
        message "Upgrade failed, and item broke. :(\n";
        $result = 'failure';
    }
    logResult( $todo, $result );
}

sub logResult {
    my ( $todo, $result ) = @_;
    my $log = File::Spec->catfile( $Settings::logs_folder, sprintf '%s_%s_%s.txt', 'refine_log', $config{username}, $config{char} );
    my $line = join(    #
        ',',
        scalar localtime(),
        $todo->{item}->{nameID},
        $todo->{upgrade} + 1,
        Misc::itemNameSimple( $todo->{item}->{nameID} ),
        $todo->{metal}->name,
        $result,
        $field->baseName,
        $char->{pos}->{x},
        $char->{pos}->{y},
    );
    open my $fp, '>>', $log;
    print $fp "$line\n";
    close $fp;
}

sub onEquipItem {
    my ( undef, $args ) = @_;

    my $item = $char->inventory->getByServerIndex( $args->{index} );
    return if !$item;
    return if !$item->{name};
    return if !$item->{equipped};
    return if $item->{name} ne $todo->{item}->{name};

    event( 'equip_item' );
}

sub onUnequipItem {
    my ( undef, $args ) = @_;

    my $item = $char->inventory->getByServerIndex( $args->{index} );
    return if !$item;
    return if !$item->{name};
    return if $item->{equipped};
    return if $item->{name} ne $todo->{item}->{name};

    event( 'unequip_item' );
}

sub onSkillUse {
    my ( undef, $args ) = @_;

    return if $args->{sourceID} ne $accountID;
    return if $args->{targetID} ne $accountID;

    my $skill = Skill->new( idn => $args->{skillID} );
    return if !$skill;
    return if $skill->getName ne $todo->{useSkill};

    event( 'skill_use' );
}

sub onUpgradeMessage {
    my ( undef, $args ) = @_;
    event( 'upgrade_message' );
}

sub onUnitLevelup {
    my ( undef, $args ) = @_;
    return if $args->{ID} ne $accountID;
    event( 'unit_levelup' );
}

sub transition {
    my ( $todo, $state, $timeout, $events ) = @_;

    $timeout ||= 0;
    $events  ||= ['timeout'];

    Log::message( "[refine] state: $todo->{state} => $state ($timeout) => @$events\n" );

    $todo->{state}       = $state;
    $todo->{event}       = '';
    $todo->{want_events} = { map { $_ => 1 } @$events };
    $todo->{timeout}     = { time => time, timeout => max( 0, $timeout ) };
}

sub event {
    my ( $event ) = @_;

    my $todo = get_todo();
    return if !$todo;

    return if !$todo->{want_events}->{$event};

    # First event wins.
    return if $todo->{event};

    Log::message( "[refine] event: $event\n" );

    $todo->{event} = $event;
    $todo->{timeout}->{time} = 0;
}

sub get_todo {
    my $i = AI::findAction( 'refine_plugin' );
    return if $i eq '';
    AI::args( $i );
}

return 1;
