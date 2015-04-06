############################################################
#
# Forge
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# 

package OpenKore::Plugin::Forge;

use strict;
use Plugins;
use Globals;
use Utils;
use Log qw(message error);
use Network::Send;
use Settings;
use FileParsers;
use Task::UseSkill;
use Misc;

Plugins::register('forge', 'Enables Forge Commands', \&Unload);

my $hooks = Plugins::addHooks(
    [ 'AI_pre'               => \&AutoForge,      undef ],
    [ 'Command_post'         => \&onCommandPost,  undef ],
    [ 'packet/refine_result' => \&onRefineResult, undef ],
    [ 'packet/skill_msg'     => \&onRefineResult2, undef ],
    [ 'packet_pre/inventory_item_added'   => \&onInventoryItemAdded ],
);

our $last_forge;
our @missing;
our $missing;

# No need to go any faster than 1 iteration per second
our $default_timeout     = 1.5;
our $default_timeoutRand = 1.75;
our $timeout ||= $timeout{forge}{timeout} || $default_timeout;
our $timeoutRand ||= $timeout{forge}{timeout} / 2 || $default_timeoutRand;
$timeout{forge}{timeout} = $timeout;
$timeout{forge}{time}    = time;

sub Unload {
	Plugins::delHooks($hooks);
}

sub AutoForge {
    return if AI::action ne 'Forge';
    return if !timeOut( $timeout{forge} );
    $timeout{forge}{time} = time;
    $timeout{forge}{timeout} = $timeout + rand $timeoutRand;

    my $args = AI::args;

    if ( $args->{done} ) {
        AI::dequeue;
        return;
    }

    if ( $args->{get_from_cart} ) {
        $args->{waiting_for_item} = 0;
        foreach ( @{ $args->{get_from_cart} } ) {
            next if $_->{requested};
            my $item = Match::cartItem( $_->{name} );
            if ( !$item || $_->{amount} > $item->{amount} ) {
                Log::error( "Canceling forge due to missing item [$_->{name}] in cart.\n" );
                $args->{done} = 1;
                return;
            }
            $messageSender->sendCartGet( $item->{index}, $_->{amount} );
            $timeout{forge}{timeout} = 3;
            $_->{requested} = 1;
            $args->{waiting_for_item} = 1;
            return; 
        }
        delete $args->{get_from_cart};
        return;
    }

    if ( $args->{amount} ne 'all' && $args->{count} >= $args->{amount} ) {
        $args->{done} = 1;
        return;
    }

    return if !CanForge( $args->{item} ) && get_from_cart( $args->{item} );

    if ( !CanForge( $args->{item} ) ) {
        CanForge( $args->{item}, 1 ) if !$args->{count};
        $args->{done} = 1;
        return;
    }

    # Only auto-forge when nobody is around to see it.
#    foreach my $player ( @{ $playersList->getItems } ) {
#        next if $overallAuth{ $player->name };
#        Log::warning( "Canceling forge due to presence of unauthorized player $player->{name}.\n" );
#        $args->{done} = 1;
#        return;
#    }

    Forge( $args->{item} );
    $args->{count}++;
}

sub onInventoryItemAdded {
    return if AI::action ne 'Forge';
    return if !AI::args->{waiting_for_item};
    $timeout{forge}{timeout} = 0;
}

##### COMMAND
sub onCommandPost {
    my ( undef, $args ) = @_;
    my ( $cmd, $subcmd, $amount ) = $args->{input} =~ /^(forge)\s*(?:(.+?)\s*(?:\s+(\d+|all))?)?$/o;

    return if !$cmd || $cmd ne 'forge';
    $args->{return} = 1;

    return cmdList( $amount ) if !$subcmd || $subcmd eq 'list';

    # Checks inventory for the items needed to forge.
    # TODO: Check cart as well, for autoforge?
    if ( $subcmd =~ /^check/ ) {
        my ( undef, $item ) = split /\s+/, $subcmd, 2;
        $config{forge_check} = CanForge( $item, 0, 1 );
        return;
    }

    if ( $subcmd && $subcmd !~ /^\d+$/ ) {
        foreach ( 0 .. 1000 ) {
            last if !$config{"forge_$_"};
            next if $config{"forge_$_"} ne $subcmd;
            $subcmd = $_;
            last;
        }
    }

    if ( $config{"forge_$subcmd"} ) {
        if ( $amount =~ /^(all|\d+)$/o ) {
            AI::queue( 'Forge', { item => $subcmd, amount => $amount, count => 0 } );
        } else {
            Forge($subcmd);
        }
    }
}

sub cmdList {
    my ( $pattern ) = @_;
    $pattern = '&no&match&' if !$pattern;
    $pattern = '.*' if $pattern eq 'all';
    for ( my $i = 0 ; exists $config{"forge_$i"} ; $i++ ) {
        my $item = $config{"forge_$i"};
        if ( CanForge( $i, 0, 1 ) ) {
            message "$i $item\n";
        } elsif ( $pattern && $item =~ /$pattern/is ) {
            message "$i $item (missing " . join( ', ', @missing ) . ")\n";
        }
    }
}

sub CanForge {
    my ( $num, $print, $ignore_status ) = @_;

    if ( $num !~ /^\d+$/o ) {
        for ( my $i = 0 ; defined $config{"forge_$i"} ; $i++ ) {
            next if $config{"forge_$i"} ne $num;
            $num = $i;
            last;
        }
    }

    @missing = ();
    $missing = { item => {}, skill => {}, status => {} };

    my $prefix = "forge_${num}";
    my $error  = 0;

    # Check skill.
    my $skill = Skill->new( name => lc( $config{"${prefix}_skill"} ) );
    if ( !binFind( \@skillsID, $skill->getHandle ) ) {
        error "Missing required skill " . $skill->getName . "\n" if $print;
        push @missing, "skill " . $skill->getName;
        $missing->{skill}->{$skill->getName}++;
        $error++;
    }

    if ( $config{"${prefix}_useSkill"} ) {
        my $useSkill = Skill->new( name => lc( $config{"${prefix}_useSkill"} ) );
        if ( !binFind( \@skillsID, $skill->getHandle ) ) {
            error "Missing required skill " . $skill->getName . "\n" if $print;
            push @missing, "skill " . $skill->getName;
            $missing->{skill}->{$skill->getName}++;
            $error++;
        }
    }

    # Check status.
    if ( !$ignore_status && $config{"${prefix}_whenStatusActive"} ) {
        my $statuses = { %{ $char->{statuses} || {} } };
        $statuses->{ $statusName{$_} } = 1 foreach keys %$statuses;
        foreach my $status ( split /\s*,\s*/, $config{"${prefix}_whenStatusActive"} ) {
            next if $statuses->{$status};
            error "Missing required status " . $status . "\n" if $print;
            push @missing, "status $status";
            $missing->{skill}->{$status}++;
            $error++;
        }
    }

    # Check items.
    for ( my $i = 0 ; exists $config{"${prefix}_item_$i"} ; $i++ ) {
        my ( $name, $amount ) = $config{"${prefix}_item_$i"} =~ /^\s*(.*)\s+(\d+)\s*$/o;
        my $item = Actor::Item::get($name);
        next if defined $item && $item->{amount} >= $amount;
        push @missing, "${amount}x $name";
        $missing->{item}->{$name} += $amount;
        error "Missing required item $name x $amount\n" if $print;
        $error++;
    }
    if ( $config{"${prefix}_useItem"} ) {
        my $item = $config{"${prefix}_useItem"};
        if ( !defined Actor::Item::get($item) ) {
            push @missing, $item;
            $missing->{item}->{$item}++;
            error "Missing required item $item x 1\n" if $print;
            $error++;
        }
    }
    foreach my $i ( 1 .. 3 ) {
        next if !$config{"forge_${num}_slot_$i"};
        my $item = $config{"forge_${num}_slot_$i"};
        if ( !defined Actor::Item::get($item) ) {
            push @missing, $item;
            $missing->{item}->{$item}++;
            error "Missing required item $item x 1\n" if $print;
            $error++;
        }
    }

    return $config{forge_check} = ( $error ? 0 : 1 );
}

sub get_from_cart {
    my ( $num ) = @_;

    # We can't fix missing skills or status.
    return if keys %{ $missing->{skill} };
    return if keys %{ $missing->{status} };

    return if !$config{"forge_${num}_getMaterialsFromCart"};

    # See if the missing items can be pulled from the cart.
    my $get_from_cart = [];
    foreach my $name ( sort keys %{ $missing->{item} } ) {
        my $amount = $missing->{item}->{$name};
        foreach ( @{ $cart{inventory} } ) {
            next if !$_ || !%$_;
            next if $_->{name} ne $name;
            my $n = $amount < $_->{amount} ? $amount : $_->{amount};
            $amount -= $n;
            push @$get_from_cart, { name => $name, amount => $n };
        }
        delete $missing->{item}->{$name} if !$amount;
    }

    return if keys %{ $missing->{item} };
    return if !@$get_from_cart;

    if ( AI::action eq 'Forge' && AI::args->{item} == $num && !AI::args->{done} ) {
        AI::args->{get_from_cart} = $get_from_cart;
    } else {
        AI::queue( 'Forge', { get_from_cart => $get_from_cart, item => $num, amount => 1, count => 0 } );
    }
    $timeout{forge}{timeout} = 0;

    return 1;
}

sub Forge {
    my $num = shift;

    my $itemName = $config{"forge_$num"};
    if ( !$itemName ) {
        message "use forge list for a list of items you can forge.\n";
        return;
    }

    return if !CanForge( $num ) && get_from_cart( $num );

    if ( !CanForge( $num, 1 ) ) {
        message "missing forging materials: " . join( ', ', @missing ) . "\n";
        return;
    }

    my $forge = {
        result => $itemName,
        lv_job => $char->{lv_job},
        dex    => $char->{dex} + $char->{dex_bonus},
        int    => $char->{int} + $char->{int_bonus},
        luk    => $char->{luk} + $char->{luk_bonus},
        items  => [],
        slots  => [],
    };

    # Use item if necessary.
    if ( $config{"forge_${num}_useItem"} ) {
        Actor::Item::get( $config{"forge_${num}_useItem"} )->use;
        push @{$forge->{items}}, $config{"forge_${num}_useItem"};
    }

    # Use skill if necessary.
    if ( $config{"forge_${num}_useSkill"} ) {
        my $skill = new Skill( auto => $config{"forge_${num}_skill"} );
        $messageSender->sendSkillUse( $skill->getIDN, $skill->getLevel, $accountID );
    }

    for ( my $i = 0 ; exists $config{"forge_${num}_item_$i"} ; $i++ ) {
        my ( $name, $amount ) = $config{"forge_${num}_item_$i"} =~ /^\s*(.*?)\s+(\d+)\s*$/o;
        my $item = Actor::Item::get($name);
        next if !$item;
        push @{$forge->{items}}, $config{"forge_${num}_item_$i"};
    }

    # Prepare to log the forge result.
    $last_forge = $forge;

    if ( $config{"forge_${num}_skill"} eq 'Change Material' ) {
        my $items = [];
        foreach ( @{ $forge->{items} } ) {
            my ( $name, $amount ) = $forge->{items}->[0] =~ /^\s*(.*?)\s+(\d+)\s*$/o;
            my $item = Actor::Item::get( $name );
            next if !$item;
            push @$items, { item => $item, amount => $amount };
        }
        my $packet = pack(    #
            'v C xxxxx vv vv', 0x07E4, 0x10,
            ( $items->[1] ? $items->[1]->{item}->{index} : 1 ), ( $items->[1] ? $items->[1]->{amount} : 0 ),    #
            $items->[0]->{item}->{index}, $items->[0]->{amount},
        );
        $messageSender->sendToServer( $packet );
        return;
    }

    # Get IDs of items to be used.
    my @itemIDs;
    if ( !$config{"forge_${num}_itemID"} ) {
        my $itemID = itemNameToID($itemName);
        if ( !$itemID ) {
            print "Unable to find ID for item $itemName.\n";
            return;
        }
        $config{"forge_${num}_itemID"} = $itemID;
    }
    push @itemIDs, $config{"forge_${num}_itemID"};
    foreach my $i ( 1 .. 3 ) {
        next if !$config{"forge_${num}_slot_$i"};
        push @itemIDs, itemNameToID( $config{"forge_${num}_slot_$i"} );
        push @{$forge->{items}}, $config{"forge_${num}_slot_$i"};
        push @{$forge->{slots}}, $config{"forge_${num}_slot_$i"};
    }

    # Send forge request.
    $messageSender->sendProduceMix( @itemIDs );

    # Setup timeout for the next forge.
    $timeout{forge}{timeout} = ( $config{"forge_${num}_timeout"} || $default_timeout ) + rand( $config{"forge_${num}_timeoutRand"} || $default_timeoutRand );
}

sub onRefineResult {
    my ( undef, $args ) = @_;
    return if !$last_forge;
    return if $args->{fail} !~ /^(0|1)$/o;

    # Deduct the items we used during the forge (if any), since the 
    # server doesn't do it for us.
    foreach my $slot ( @{$last_forge->{slots}} ) {
        my $item = Actor::Item::get($slot);
        next if !$item;
        inventoryItemRemoved( $item->{invIndex}, 1 );
    }

    # Log the result.
    $last_forge->{success} = $args->{fail} ? 'failure' : 'success';
    logResult( $last_forge );

    $last_forge = undef;
}

sub onRefineResult2 {
    my ( undef, $args ) = @_;
    return if !$last_forge;

    # HACK: Only listen for the result of Change Material.
    return if $args->{id} != 2494;

    # Deduct the items we used during the forge (if any), since the
    # server doesn't do it for us.
    foreach my $slot ( @{ $last_forge->{slots} } ) {
        my $item = Actor::Item::get( $slot );
        next if !$item;
        inventoryItemRemoved( $item->{invIndex}, 1 );
    }

    # Log the result.
    $last_forge->{success} = $args->{msgid} == 1574 ? 'success' : 'failure';
    logResult( $last_forge );

    $last_forge = undef;
}

sub itemNameToID {
    my $name = lc $_[0];

    # First try to look the item up in our inventory.
    my $item = Actor::Item::get($name);
    return $item->{nameID} if $item;

    # scan the entire items.txt file (this is slow)
    foreach ( keys %items_lut ) {
        next if lc $items_lut{$_} ne $name;
        return $_;
    }

    return;
}

sub logResult {
    my ($forge) = @_;
    my $log =
      File::Spec->catfile( $Settings::logs_folder, sprintf '%s_%s_%s.txt',
        'forge_log', $config{username}, $config{char}, );
    my $line = join( ',',
        scalar localtime(),
        $forge->{result}, $forge->{lv_job}, $forge->{dex}, $forge->{luk}, $forge->{int}, 
        @{$forge->{items}}, $forge->{success} );
    open my $fp, ">>$log";
    print $fp "$line\n";
    close $fp;
}

return 1;
