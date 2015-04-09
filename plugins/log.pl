package OpenKore::Plugin::Log;

# Plugin to log various things to the log folder and/or mysql.
#
# players_LOGIN_CHARINDEX.txt  - Information on player sightings.
# chatroom_LOGIN_CHARINDEX.txt - chatroom titles and locations
# buyers_LOGIN_CHARINDEX.txt   - "buyer" stores

use strict;

use Globals;
use Log qw(message warning error);
use Misc;
use Plugins;
use Utils;

#use Devel::Size;
#use Devel::Symdump;
use JSON;
use List::Util qw( &sum );
use Text::CSV_XS;
use Time::HiRes qw(time);
use YAML::Syck;

Plugins::register( 'log', 'Log various things', \&Unload, \&Reload );

my $hooks = Plugins::addHooks(    #
    [ start3                               => \&checkConfig ],
    [ mainLoop_pre                         => \&checkState ],
    [ TODO_mapChange                       => \&checkState ],
    [ 'packet/inventory_item_added'        => sub { logInventory( 1 ); } ],
    [ 'packet/inventory_item_removed'      => sub { logInventory( 1 ); } ],
    [ 'packet/item_used'                   => sub { logInventory( 1 ); } ],
    [ 'packet/inventory_item_stackable'    => \&logInventory ],
    [ 'packet/inventory_item_nonstackable' => \&logInventory ],
    [ 'packet/deal_complete'               => \&logInventory ],

    [ 'packet/storage_item_added'        => sub { logStorage( 1 ); } ],
    [ 'packet/storage_item_removed'      => sub { logStorage( 1 ); } ],
    [ 'packet/storage_item_stackable'    => \&logStorage ],
    [ 'packet/storage_item_nonstackable' => \&logStorage ],

    [ 'packet/cart_item_added'        => sub { logCart( 1 ); } ],
    [ 'packet/cart_item_removed'      => sub { logCart( 1 ); } ],
    [ 'packet/cart_item_stackable'    => \&logCart ],
    [ 'packet/cart_item_nonstackable' => \&logCart ],

    [ 'packet/buying_store_found' => \&logBuyerStore ],

    [ 'player_exist' => \&onPlayerExist ],

    [ 'packet_attack'   => \&onPacketAttack ],
    [ 'packet_skilluse' => \&onPacketSkillUse ],

    [ 'packet_localBroadcast' => \&logLocalBroadcast ],

    [ 'packet/friend_logon' => \&logFriend ],

    [ 'packet/guild_member_online_status' => \&logGuildMember ],
    [ 'packet/guild_location'             => \&logGuildMember ],
    [ 'packet/guild_members_list'         => \&logGuildMembers ],

    [ 'npc_exist' => \&onNPCExist ],
    [ 'npc_talk'  => \&onNPCTalk ],

    [ 'packet/stat_info'  => \&onStatInfo ],
    [ 'packet/stat_info2' => \&onStatInfo ],

    [ 'packet_pre/character_equip' => \&onPacketPreCharacterEquip ],
    [ 'packet_character_equip'     => \&onPacketCharacterEquip ],

    [ 'packet/actor_action' => \&onActorAction ],
    [ 'packet/actor_moved'  => \&onActorMoved ],

    [ 'Network::stateChanged' => \&onNetworkStateChanged ],

    [ 'packet_pre/sense_result' => \&onSenseResult ],

    [ 'spread_pm' => \&onSpreadPM ],

    [ 'packet_chatinfo' => \&logChat ],

    [ 'packet/quest_all_mission'         => \&logQuests ],
    [ 'packet/quest_all_mission_v2'      => \&logQuests ],
    [ 'packet/quest_update_mission_hunt' => \&logQuests ],
);

sub logLocalBroadcast {
    my ( undef, $params ) = @_;
    if ( $params->{Msg} =~ /King Poring/ ) {
        # Report the number of Poring Coins too.
        my ( $coins ) = grep { $_->{name} eq 'Poring Coin' } @{ $char->inventory->getItems };
        $coins = $coins ? $coins->{amount} : 0;

        eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => "$params->{Msg} Currently have [$coins] Poring Coins." } ); };

        # Probably the right thing to do here is:
        # 1. Expand our lockMap box to 10 or so for 3 minutes.
        # 2. Reduce our attackDistance and run a tour. (Because Porings eat the coins.)
        # 3. Restore our attackDistance and lockMap spot until the next time King Poring spawns.
        # This looks a lot more like a macro than a plugin.

        # The problem with logging out is that we'll lose our spot.
        if ( $params->{Msg} =~ /The King Poring has been destroyed spilling coins all over the field/ ) {
            my ( $prefix ) = map { s/_label$//;$_ } grep { /_label$/o && $config{$_} eq 'break_tmp' } %config;
            if ( $prefix ) {
                Misc::configModify( "${prefix}_startTime" => sprintf '%02d:%02d:%02d', ( localtime( time + 300 ) )[ 2,  1, 0 ] );
                Misc::configModify( "${prefix}_stopTime" => sprintf '%02d:%02d:%02d', ( localtime( time + 1500 ) )[ 2, 1, 0 ] );
                Misc::configModify( "${prefix}_disabled" => 0 );
            }
        }
    }
}

sub logBuyerStore {
    my ( undef, $params ) = @_;
    my $id    = unpack 'V', $params->{ID};
    my $buyer = $players{ $params->{ID} };
    my $pos   = $buyer->{pos};
    Log::message( "STORE [$id] [$params->{title}] [$pos->{x},$pos->{y}]\n" );
    dumpBuyer(
        {
            id    => unpack( 'V', $params->{ID} ),
            name  => $buyer->{name},
            party => $buyer->{party},
            guild => $buyer->{guild},
            field => $field->name,
            pos   => $buyer->{pos},
            title => $params->{title},
        }
    );
}

our $reloading;

our $csv       = Text::CSV_XS->new( { binary => 1 } );
our $players   = {};
our $monsters  = {};
our $log_files = {};
our $friends   = {};
our $guildies  = {};

our $inventory_cache = [];
our $storage_cache   = [];
our $cart_cache      = [];

our $stat_update = { timeout => { timeout => 3 } };
our $char_equip  = { timeout => { timeout => 5 } };
our $memlog      = { timeout => { timeout => 10 }, details_timeout => { timeout => 119 } };

our $shop_state;
our $tombs = {};
our $field_name;

our $config_defaults = {
    logFriends                                 => undef,
    logFriendsBlackList                        => undef,
    logFriendsFlappingSuppressionInterval      => 600,
    logPlayersInLockMap                        => undef,
    logTomb                                    => undef,
    logEquipment                               => 1,
    logAutoRequestEquipment                    => undef,
    logAutoRequestEquipmentTimeout             => 86400,
    logEvent_sit                               => undef,
    logEvent_online                            => 1,
    logGuild                                   => 1,
    logGuildMembersBlackList                   => undef,
    logGuildMembersFlappingSuppressionInterval => 600,
    logGuild_memberUpdateTimeout               => 600,
};

# Pre-declare dbh so we can use it without parentheses.
sub dbh;

checkConfig() if $reloading;

sub checkConfig {
    foreach ( sort keys %$config_defaults ) {
        next if exists $config{$_};
        configModify( $_ => $config_defaults->{$_} );
    }
}

sub Unload {
    Plugins::delHooks( $hooks );
    dumpPlayer( $_ ) foreach values %$players;
    $players   = {};
    $log_files = {};
    message "log plugin unloaded.\n";
}

sub Reload {
    Unload();
    $reloading = 1;
}

sub onNPCExist {
    return if !$_[1] || ref $_[1] ne 'HASH' || !$_[1]->{npc};
    my $npc = $_[1]->{npc};
    next if $npc !~ /Tomb/;
    next if !$config{logTomb} && !$config{mvpMode};
    my $tomb = $tombs->{ $npc->{ID} } ||= { id => $npc->{ID}, pos => { %{ $npc->{pos} }, map => $field->baseName } };

    # Ignore tombs we already logged recently.
    return if $tomb->{timeout} && !timeOut( $tomb->{timeout} );

    # Minimum of 60 seconds between reports of the same tomb.
    # It's really unlikely that an MVP would spawn and die on exactly the same spot in less than 60 seconds.
    $tomb->{timeout} = { time => time, timeout => 60 };

    eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => sprintf 'Saw NPC [%s] at (%d %d %s).', $npc->name, $npc->{pos}->{x}, $npc->{pos}->{y}, $field->name } ); };

    # Attempt to identify who killed the MVP and when it died.
    $tomb->{line} = 0;
    $messageSender->sendTalk( $npc->{ID} );
}

sub onNPCTalk {
    my ( undef, $params ) = @_;
    return if !$params || ref $params ne 'HASH' || !$params->{ID};

    my $tomb = $tombs->{ $params->{ID} };
    return if !$tomb;

    $tomb->{line}++;

    if ( $tomb->{line} == 1 && $params->{msg} =~ /\[\s*(.*?)\s*\]/o ) {
        $tomb->{mvp_name} = $1;
    }

    if ( $tomb->{line} == 3 && $params->{msg} =~ /Time of death : (\d+):(\d+)/o ) {
        $tomb->{time_of_death} = sprintf '%02d:%02d', $1, $2;
    }

    if ( $tomb->{line} == 5 && $params->{msg} =~ /\[\s*(.*?)\s*\]/o ) {
        $tomb->{killer} = $1;
        dumpMVP( $tomb );
        eval {
            OpenKore::Plugin::Spread::send_log(
                {
                    switch  => 'notice',
                    message => sprintf 'MVP [%s] was killed by [%s] at [%s] at [%d %d %s].', $tomb->{mvp_name}, $tomb->{killer}, $tomb->{time_of_death}, $tomb->{pos}->{x}, $tomb->{pos}->{y}, $tomb->{pos}->{map},
                }
            );
        };
    }
}

sub logInventory {
    my ( $allow_remove ) = @_;

    my $old_list = $inventory_cache;
    my $new_list = [];

    foreach my $item ( @{ $char->inventory->getItems } ) {
        next if !$item;
        $new_list->[ $item->{invIndex} ] = {
            name     => $item->{name},
            nameID   => $item->{nameID},
            upgrade  => $item->{upgrade} || 0,
            cards    => $item->{cards} || '',
            amount   => $item->{amount},
            equipped => $item->{equipped} ? 1 : 0,
        };
use JSON;
#Log::message( encode_json( { map { $_ => $item->{$_} } grep { !ref $item->{$_} } keys %$item } ) ) if $item;
#Log::message( encode_json( $new_list->[ $item->{invIndex} ] ) ) if $item;
    }

    logItems( 'inventory', $old_list, $new_list, $allow_remove );

    @$old_list = @$new_list;
}

sub logItems {
    my ( $location, $old_list, $new_list, $allow_remove ) = @_;

    foreach ( @$new_list ) {
        next if !$_;
        $_->{key} = "$_->{name}|$_->{amount}|$_->{equipped}";
    }

    foreach ( 0 .. $#$new_list ) {
        my $item = $new_list->[$_];
        next if !$item;
        next if $old_list->[$_] && $old_list->[$_]->{key} eq $item->{key};
        my $table_data = item_to_table_data( $item, 'array' );
#Log::message( encode_json( [ $char->{name}, $location, $_, $item->{name}, $item->{amount}, $item->{equipped}, @$table_data ] ) );
        dbh->do(
            'insert into iro_inventory (ctime,mtime,char_name,location,bin_id,item_name,amount,equipped,item_id,refine,card1_id,card2_id,card3_id,card4_id,crafted_by,star_crumb)'
                . ' values (now(),now(),?,?,?,?,?,?,?,?,?,?,?,?,?,?)'
                . ' on duplicate key update mtime=now(),item_name=values(item_name),amount=values(amount),equipped=values(equipped),'
                . 'item_id=values(item_id),refine=values(refine),card1_id=values(card1_id),card2_id=values(card2_id),card3_id=values(card3_id),card4_id=values(card4_id),'
                . 'crafted_by=values(crafted_by),star_crumb=values(star_crumb)',
            undef,
            $char->{name},
            $location,
            $_,
            $item->{name},
            $item->{amount},
            $item->{equipped},
            @$table_data,
        );
    }

    if ( $allow_remove ) {
        my @keep = grep { $new_list->[$_] } 0 .. $#$new_list;
        my $qs = join ',', ( '?' ) x @keep;
        dbh->do( "delete from iro_inventory where char_name=? and location=? and bin_id not in ($qs)", undef, $char->{name}, $location, @keep ) if @keep;
    }
}

sub item_to_table_data {
    my ( $item, $as ) = @_;

    my @cards = unpack 'vvvv', $item->{cards};
    my $table_data = {    #
        item_id    => $item->{nameID},
        refine     => $item->{upgrade},
        card1_id   => $cards[0],
        card2_id   => $cards[1],
        card3_id   => $cards[2],
        card4_id   => $cards[3],
        crafted_by => 0,
        element    => 'none',
        star_crumb => 0,
    };

    # Forged and brewed items.
    if ( $cards[0] == 255 ) {
        $table_data->{crafted_by} = unpack 'V', substr $item->{cards}, 4;
        $table_data->{element}    = $cards[1] % 10;
        $table_data->{star_crumb} = $cards[1] / 1280;
    }

    if ( $as && $as eq 'array' ) {
        $table_data = [ map { $table_data->{$_} } qw( item_id refine card1_id card2_id card3_id card4_id crafted_by star_crumb ) ];
    }

    return $table_data;
}

sub logStorage {
    my ( $allow_remove ) = @_;

    my $old_list = $storage_cache;
    my $new_list = [];

    for ( my $i = 0 ; $i < @storageID ; $i++ ) {
        next if !$storageID[$i];
        my $item = $storage{ $storageID[$i] };
        $new_list->[$i] = {
            name     => $item->{name},
            nameID   => $item->{nameID},
            upgrade  => $item->{upgrade} || 0,
            cards    => $item->{cards} || '',
            amount   => $item->{amount},
            equipped => 0,
        };
    }

    logItems( 'storage', $old_list, $new_list, $allow_remove );

    @$old_list = @$new_list;
}

sub logCart {
    my ( $allow_remove ) = @_;

    my $old_list = $cart_cache;
    my $new_list = [];

    for ( my $i = 0 ; $i < @{ $cart{'inventory'} } ; $i++ ) {
        my $item = $cart{inventory}[$i];
        next if !$item;
        next if !%$item;
        $new_list->[$i] = {
            name     => $item->{name},
            nameID   => $item->{nameID},
            upgrade  => $item->{upgrade} || 0,
            cards    => $item->{cards} || '',
            amount   => $item->{amount},
            equipped => 0,
        };
    }

    logItems( 'cart', $old_list, $new_list, $allow_remove );

    @$old_list = @$new_list;
}

sub logFriend {
    my ( undef, $args ) = @_;

    my ( $friend ) = grep { $_->{accountID} eq $args->{friendAccountID} && $_->{charID} eq $args->{friendCharID} } values %friends;
    return if !$friend;

    my $action = $args->{isNotOnline} ? 'disconnected' : 'connected';

    return if !$config{logFriends};

    $csv->combine( scalar( localtime() ), $friend->{name}, $action );
    logit( 'friends', $csv->string . "\n" );

    return if $config{logFriends} < 2;
    return if $config{logFriendsBlackList} && $config{logFriendsBlackList} =~ /(^|,)\Q$friend->{name}\E(,|$)/;
    
    if ( $config{logFriendsFlappingSuppressionInterval} ) {
        my $f = $friends->{ $friend->{name} } ||= {};
        $f->{$action} = time;
        return if !$f->{disconnected_msg_sent};
        delete $f->{disconnected_msg_sent};
    }

    eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => "Friend [$friend->{name}] $action" } ); };
}

sub logGuildMembers {
    my ( undef, $args ) = @_;
    foreach ( @{ $guild{member} } ) {
        logGuildMember( undef, { charID => $_->{charID} } );
    }
}

sub logGuildMember {
    my ( undef, $args ) = @_;

    my $key = $args->{charID} ? 'charID' : 'ID';
    my ( $guildie ) = grep { $_->{$key} eq $args->{$key} } @{ $guild{member} || [] };
    return if !$guildie;

    my $action = 'info';
    $action = $args->{online} ? 'connected' : 'disconnected' if exists $args->{online};

    return if !$config{logGuild};

    my @data = (
        $guildie->{name},
        $action,
        $guild{name},
        $guildie->{lv},
        $jobs_lut{ $guildie->{jobID} },
        $guildie->{contribution},
        $guildie->{title},
        ( $guildie->{pos} || {} )->{x},
        ( $guildie->{pos} || {} )->{y},
    );
    $csv->combine( @data );
    my $status = $csv->string;
    my $f = $guildies->{ $guildie->{name} } ||= {};
    if ( $action ne 'info' || !$f->{status} || $f->{status} ne $status ) {
        $f->{status} = $status;
        $csv->combine( scalar( localtime() ), @data );
        logit( 'guild', $csv->string . "\n" );
    }

    return if $action eq 'info';
    return if $config{logGuild} < 2;
    return if $config{logGuildMembersBlackList} && $config{logGuildMembersBlackList} =~ /(^|,)\Q$guildie->{name}\E(,|$)/;
    
    if ( $config{logGuildMembersFlappingSuppressionInterval} ) {
        $f->{$action} = time;
        return if !$f->{disconnected_msg_sent};
        delete $f->{disconnected_msg_sent};
    }

    eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => "Guild Member [$guildie->{name}] $action" } ); };
}

sub logChat {
    my ( undef, $args ) = @_;
    return if !$args->{ownerID};
    my $player = $playersList->getByID( $args->{ownerID} );
    return if !$player;
    my $pos = calcPosition( $player );
    my $title = $args->{title};
    $title =~ s/\s+/ /gos;
    $title =~ s/^\s+|\s+$//gos;
    $csv->combine( scalar localtime(), unpack( 'V', $args->{ownerID} ), $args->{num_users}, $player->name, $pos->{x}, $pos->{y}, $field->baseName, $args->{title} );
    logit( 'chatroom', $csv->string . "\n" );
}

sub logQuests {
    our $quest_cache ||= {};

    foreach ( keys %$questList ) {
        my $quest    = $questList->{$_};
        my $missions = $quest->{missions};
        next if !$missions;
        next if !keys %$missions;
        my $state = 'complete';
        foreach ( values %$missions ) {
            next if $_->{count} && $_->{goal} && $_->{count} == $_->{goal};
            $state = 'not_started' if !$_->{count};
            next if $state eq 'not_started';
            $state = 'started';
        }
        if (!$quest_cache->{$_} || $quest_cache->{$_} ne $state) {
            if ( $quest_cache->{$_} eq 'started' && $state eq 'complete' ) {
                my $title = ${ $quests_lut{$_} || {} }{title};
                eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => "Quest $_ [$title] is complete!" } ); };
            }
            $quest_cache->{$_} = $state;
        }
    }
}

#sub pkg_list {
#    return $_[0], map { pkg_list( $_ ) } Devel::Symdump->new( $_[0] )->packages;
#}
#
#sub dump_mem {
#    # We don't care about the "compiled regex" warning, and it spams the console.
#    $Devel::Size::warn = 0;
#
#    my @pkgs = pkg_list( 'main' );
#
#    my $seen = {};
#    my $mem = { AA_time => time, AA_time_str => scalar( localtime() ), AA_details => {} };
#    foreach my $top_pkg ( sort @pkgs ) {
#        my $obj = Devel::Symdump->new( $top_pkg );
#        foreach my $type ( qw( packages scalars arrays hashes functions ios ) ) {
#            my $sigil = '';
#            $sigil = '$' if $type eq 'scalars';
#            $sigil = '@' if $type eq 'arrays';
#            $sigil = '%' if $type eq 'hashes';
#            $sigil = '&' if $type eq 'functions';
#            foreach my $name ( sort $obj->$type ) {
#
#                # Skip names which will cause eval to log warning messages.
#                next if $name =~ /[^\w:]/os;
#
#                my $pkg = ( $name =~ /^(.*)::/o )[0] || $name;
#                my $var = eval( "\\$sigil$name" );
#                next if !$var;
#                next if $seen->{$var};
#                $seen->{$var}++;
#                my $size = Devel::Size::total_size( $var );
#                $mem->{$pkg}->{$type} += $size;
#                if ( ( 1 || $pkg eq 'AI' ) && ( $type eq 'scalars' || $type eq 'hashes' ) ) {
#                    $mem->{AA_details}->{"$sigil$name"} = $size;
#                }
#            }
#        }
#    }
#
#    return $mem;
#}

sub checkState {
    my ( undef, $args ) = @_;

    if ( timeOut( $memlog->{timeout} ) ) {
        my $fp;
        open $fp, "/proc/$$/statm";
        my $memstr = <$fp>;
        close $fp;
        my $mem = $memstr * 4;
        if ( $memlog->{mem} != $mem ) {
            $memlog->{mem} = $mem;
            $csv->combine( scalar( localtime() ), time, $mem, $$ );
            logit( 'memory', $csv->string . "\n" );
        }

#        if ( $config{logMemoryDetails} && timeOut( $memlog->{details_timeout} ) ) {
#            logit( 'memory_details', YAML::Syck::Dump( dump_mem() ) );
#            $memlog->{details_timeout}->{time} = time;
#        }

        $memlog->{timeout}->{time} = time;
    }

    return if !$char;
    return if !$field;

    if ( !$field_name || $field_name ne $field->baseName ) {
        log_event( map_leave => $field_name ) if $field_name;
        log_event( lockMap => 'leave' ) if $field_name && $field_name eq $config{lockMap};
        $field_name = $field->baseName;
        log_event( lockMap => 'enter' ) if $field_name eq $config{lockMap};
        log_event( map_enter => $field_name );
    }

    if ( $guild{member} && $config{logGuild} && $config{logGuild_memberUpdateTimeout} ) {
        our $last_guild_update ||= 0;
        if ( timeOut( $last_guild_update, $config{logGuild_memberUpdateTimeout} ) ) {
            $last_guild_update = time;
            if ( $net->getState == Network::IN_GAME ) {
                Log::message( "[log] requesting guild member info\n" );
                $messageSender->sendGuildRequestInfo( 1 );
            }
        }
    }

    our $sp_old ||= 0;
    our $hp_old ||= 0;
    if ( $char && ( $char->{hp} != $hp_old || $char->{sp} != $sp_old )) {
        my $msg = '';
        $msg .= sprintf "HP %s%d ", ($char->{hp} < $hp_old ? '-' : '+'), abs $char->{hp} - $hp_old if $char->{hp} != $hp_old;
        $msg .= sprintf "SP %s%d ", ($char->{sp} < $sp_old ? '-' : '+'), abs $char->{sp} - $sp_old if $char->{sp} != $sp_old;
        Log::message( "$msg\n" );
        $hp_old = $char->{hp};
        $sp_old = $char->{sp};
    }

    if ( $stat_update->{config} && timeOut( $stat_update->{timeout} ) ) {
        my $c = $stat_update->{config};
        foreach ( sort keys %$c ) {
            next if $config{$_} eq $c->{$_};
            configModify( $_ => $c->{$_} );
        }
        delete $stat_update->{config};
    }

    if ( $config{logFriendsFlappingSuppressionInterval} ) {
        foreach ( keys %$friends ) {
            next if $friends->{$_}->{disconnected_msg_sent};
            next if $friends->{$_}->{connected} >= $friends->{$_}->{disconnected};
            next if $config{logFriendsFlappingSuppressionInterval} > time - $friends->{$_}->{disconnected};
            $friends->{$_}->{disconnected_msg_sent}++;
            eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => sprintf "Friend [%s] disconnected at %02d:%02d:%02d", $_, ( localtime( $friends->{$_}->{disconnected} ) )[ 2, 1, 0 ] } ); };
        }
    }

    if ( $config{logGuildMembersFlappingSuppressionInterval} ) {
        foreach ( keys %$guildies ) {
            next if $guildies->{$_}->{disconnected_msg_sent};
            next if $guildies->{$_}->{connected} >= $guildies->{$_}->{disconnected};
            next if $config{logGuildMembersFlappingSuppressionInterval} > time - $guildies->{$_}->{disconnected};
            $guildies->{$_}->{disconnected_msg_sent}++;
            eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => sprintf "Guild Member [%s] disconnected at %02d:%02d:%02d", $_, ( localtime( $guildies->{$_}->{disconnected} ) )[ 2, 1, 0 ] } ); };
        }
    }

    my $new_players = {};
    foreach my $pl ( @{ $playersList->getItems() } ) {
        next if binFind( \@partyUsersID, $pl->{ID} );
        my $id = $pl->{nameID};
        my $p = $new_players->{$id} = $players->{$id} || {};
        delete $players->{$id};
        autoRequestEquipment($pl) if !%$p;
        $p->{id}         ||= $id;
        $p->{name}       ||= $pl->{name};
        $p->{job}        ||= $pl->job;
        $p->{lv}         ||= $pl->{lv};
        $p->{time_start} ||= Time::HiRes::time;
        $p->{field}      ||= $field->name;
        $p->{pos_start}  ||= $pl->{pos};
        $p->{party}      ||= $pl->{party}->{name};
        $p->{guild}      ||= $pl->{guild}->{name};
        $p->{time_end} = Time::HiRes::time;
        $p->{pos_end}  = $pl->{pos};
    }
    dumpPlayer( $_ ) foreach values %$players;
    $players = $new_players;

    my $new_monsters = {};
    foreach my $pl ( @{ $monstersList->getItems() } ) {
        my $id = unpack 'V', $pl->{ID};
        my $p = $new_monsters->{$id} = $monsters->{$id} || {};
        delete $monsters->{$id};
        $p->{id}         ||= $id;
        $p->{type}       ||= $pl->{nameID};
        $p->{name}       ||= $pl->name;
        $p->{time_start} ||= Time::HiRes::time;
        $p->{field}      ||= $field->name;
        $p->{pos_start}  ||= $pl->{pos};
        $p->{pos_end}  = $pl->{pos};
        $p->{time_end} = Time::HiRes::time;
        $p->{dmgFrom}  = $pl->{dmgFrom};
        $p->{dmgTo}    = $pl->{dmgTo};
    }
    foreach ( keys %$monsters ) {
        if ( $monsters->{$_}->{time_end} + 5 < time ) {
            dumpMonster( $monsters->{$_} );
            delete $monsters->{$_};
        } else {
            $new_monsters->{$_} = $monsters->{$_};
        }
    }
    $monsters = $new_monsters;

    if ( $config{logShopState} ) {
        my $old = $shop_state;
        $shop_state = $shopstarted ? 1 : 0;
        if ( defined $old && $old != $shop_state ) {
            my $action = $shop_state ? 'opened' : 'closed';
            eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => "Shop $action" } ); };
        }
    }

    return;
}

sub autoRequestEquipment {
    my ( $p ) = @_;

    # Only do this if we're allowed.
    return if !$config{logAutoRequestEquipment};

    # Only do this if the last time we requested the equipment was long enough ago.
    # This locks the table, so last_showeq cannot safely be added to iro_character.
    my $r = dbh->do( #
        'insert into iro_character_showeq (name,last_showeq) values (?,from_unixtime(?+(@guard:=0))) on duplicate key update last_showeq=if((@guard:=1) and (last_showeq="0000-00-00 00:00:00" or unix_timestamp(values(last_showeq))>unix_timestamp(last_showeq)+?),values(last_showeq),last_showeq)',
        undef, $p->{name}, time, $config{logAutoRequestEquipmentTimeout} || $config_defaults->{logAutoRequestEquipmentTimeout},
    );

    # If @guard=0 and $r=1, insert triggered.
    # If @guard=1 and $r=1, update triggered, but no rows updated.
    # If @guard=1 and $r=2, update triggered, and 1 row was updated.

    # Update didn't change any rows.
    return if $r == 1 && dbh->selectrow_array( 'select @guard' );

    $messageSender->sendShowEquipPlayer( $p->{ID} );
}

sub onPacketAttack {
    my ( undef, $args ) = @_;
    logAttack( Actor::get( $args->{sourceID} ) => Actor::get( $args->{targetID} ), $args->{dmg}, -( $args->{type} || 0 ) );
}

sub onPacketSkillUse {
    my ( undef, $args ) = @_;
    logAttack( Actor::get( $args->{sourceID} ) => Actor::get( $args->{targetID} ), $args->{damage}, $args->{skillID}, $args->{amount} );
}

sub logAttack {
    my ( $source, $target, $dmg, $skill, $lv ) = @_;

    return if !$source || !$target;
    return if !$source->isa( 'Actor::You' ) && !$target->isa( 'Actor::You' );

    my ( $player, $monster, $dir ) = $source->isa( 'Actor::You' ) ? ( $source, $target, 'pm' ) : ( $target, $source, 'mp' );

    return if !$monster->{type};

    my $mpos = calcPosition( $monster );
    my $ppos = calcPosition( $player );

    $lv = $lv && $lv != 65535 ? $lv : 0;

    $csv->combine(

        # Monster info.
        unpack( 'V', $monster->{ID} ),
        $monster->{type} || '',

        # Player info.
        $player->{name}   || '',
        $player->{nameID} || 0,
        $char->{jobID}    || 0,

        # Effect info.
        $dir,
        $dmg   || 0,
        $skill || 0,
        $lv,

        # Environment.
        $monster->{dmgFrom} || 0,
        $monster->{dmgTo}   || 0,
        Time::HiRes::time,
        $field->{baseName},
        $mpos->{x},
        $mpos->{y},
        $ppos->{x},
        $ppos->{y},
    );

    logit( 'attacks', $csv->string . "\n" );
}

sub onPlayerExist {
    my ( undef, $args ) = @_;
    my $p = $args->{player};
    return if !$p;
    dbh->do(    #
        'insert into iro_character (player_id,name,first_seen,last_seen,job_id,base_level,guild_id,party) values (?,?,now(),now(),?,?,?,?)'
            . ' on duplicate key update char_id=LAST_INSERT_ID(char_id),last_seen=now(),job_id=values(job_id),base_level=values(base_level),guild_id=values(guild_id),party=values(party)',
        undef, $p->{nameID}, $p->{name} || 'unknown', $p->{jobID} || 0, $p->{lv} || 0, unpack( 'V', $p->{guildID} ) || 0,
        $p->{party}->{name} || '',
    );
    my $char_id = dbh->{mysql_insertid};
    my ( $cart ) = grep {$_} map { $p->{statuses}->{"EFFECTSTATE_PUSHCART$_"} ? ( $_ || 1 ) : 0 } ( '', 2 .. 9 );
    dbh->do(
        'insert into iro_character_sighting (char_id,ctime,name,job_id,base_level,guild_id,party,map_id,x,y,gender,hair_color,hair_style,head_top,head_mid,head_low,weapon,shield,sitting,cart) values (?,now(),?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)',
        undef,
        $char_id,
        $p->{name}  || 'unknown',
        $p->{jobID} || 0,
        $p->{lv}    || 0,
        unpack( 'V', $p->{guildID} ) || 0,
        $p->{party}->{name} || '',
        get_map_id(),
        $p->{pos}->{x} || 0,
        $p->{pos}->{y} || 0,
        ( defined $p->{sex} ? $p->{sex} : -1 ) + 1,
        $p->{hair_color}      || 0,
        $p->{hair_style}      || 0,
        $p->{headgear}->{top} || 0,
        $p->{headgear}->{mid} || 0,
        $p->{headgear}->{low} || 0,
        $p->{weapon}          || 0,
        $p->{shield}          || 0,
        $p->{sitting}         || 0,
        $cart                 || 0,
    );
}

sub onStatInfo {
    my ( undef, $args ) = @_;

    return if !$char;

    foreach ( qw( lv lv_job str vit dex int agi luk str_bonus vit_bonus dex_bonus int_bonus agi_bonus luk_bonus hp_max sp_max flee crit attack_speed critical hit attack attack_bonus attack_magic_min attack_magic_max def def_bonus zeny ) ) {
        next if !$char->{$_};
        $stat_update->{config}->{"stat_$_"} = $char->{$_};
    }
    $stat_update->{config}->{"stat_job"} = $jobs_lut{ $char->{jobID} } if $char->{jobID};

    $stat_update->{timeout}->{time} = time;
}

sub onPacketPreCharacterEquip {
    my ( undef, $args ) = @_;

    return if !$config{logEquipment};

    $char_equip->{timeout}->{time} = 0;

    my ( $char_id ) = dbh->selectrow_array( 'select char_id from iro_character where name=?', undef, $args->{name} );
    return if !$char_id;

    $char_equip->{timeout}->{time} = time;
    $char_equip->{name}            = $args->{name};
    $char_equip->{char_id}         = $char_id;
}

sub onPacketCharacterEquip {
    my ( undef, $args ) = @_;

    return if timeOut( $char_equip->{timeout} );

    my $actor   = ( grep { $_->name eq $char_equip->{name} } @{ $playersList->getItems } )[0] || {};
    my $char_id = $char_equip->{char_id};
    my $item    = $args->{item};
    my @cards   = unpack 'vvvv', $item->{cards};

    dbh->do(    #
        'insert into iro_character_equipment_history (char_id,equip_mtime,equip_slot,item_id,refine,card1_id,card2_id,card3_id,card4_id,head_top,head_mid,head_low) values (?,from_unixtime(?),?,?,?,?,?,?,?,?,?,?)',
        undef, $char_id, $char_equip->{timeout}->{time}, $item->{equipped}, $item->{nameID}, $item->{upgrade}, $cards[0], $cards[1], $cards[2], $cards[3], 
        $actor->{headgear}->{top} || 0,
        $actor->{headgear}->{mid} || 0,
        $actor->{headgear}->{low} || 0,
    );
}

our $collect_data ||= {};
sub onActorAction {
    my ( undef, $params ) = @_;
    log_event( sit => 'begin' ) if $params->{type} == Network::PacketParser::ACTION_SIT;
    log_event( sit => 'end' )   if $params->{type} == Network::PacketParser::ACTION_STAND;

    if ( $params->{sourceID} && $params->{targetID} && $params->{targetID} eq $accountID && $params->{src_speed} ) {
        my $source = $monstersList->getByID( $params->{sourceID} );
        if ( $source && $collect_data->{ $source->{nameID} } && !$collect_data->{ $source->{nameID} }->{aDelay} ) {
            $collect_data->{ $source->{nameID} }->{aDelay}  = $params->{dst_speed};
            $collect_data->{ $source->{nameID} }->{aMotion} = $params->{src_speed};
            $collect_data->{ $source->{nameID} }->{name}    = $source->{name};
            onSenseResult( undef, $collect_data->{ $source->{nameID} } );
        }
    }
}

sub onActorMoved {
    my ( undef, $params ) = @_;

    return if !$params->{walk_speed};
    my $actor = $monstersList->getByID( $params->{ID} );
    return if !$actor;

    my $collect = $collect_data->{ $actor->{nameID} };
    if ( $collect && !$collect->{walk_speed} ) {
        $collect->{walk_speed}  = $params->{walk_speed};
        $collect->{name}        = $actor->{name};
        onSenseResult( undef, $collect );
    }
}

sub onNetworkStateChanged {

    # When we log in, we always start out standing.
    log_event( sit => 'end' ) if $net->getState() != Network::IN_GAME;
}

sub onSenseResult {
    my ( undef, $args ) = @_;
    my $collect = $collect_data->{ $args->{nameID} } ||= {};
    $collect->{$_} = $args->{$_} foreach keys %$args;
    $csv->combine( scalar( localtime() ), @$collect{qw( nameID level hp size race element def mdef aDelay aMotion walk_speed name )} );
    logit( 'sense', $csv->string . "\n" );
}

sub onSpreadPM {
    my ( undef, $params ) = @_;
    next if $params->{privMsgUser} ne $char->{name} && $params->{privMsgUser} ne 'ALL';

    if ( $params->{privMsg} =~ /^log rotate_logs$/ ) {
        foreach my $file ( sort keys %$log_files ) {
            my $log = File::Spec->catfile( $Settings::logs_folder, sprintf '%s_%s_%s.txt', $file, $config{username}, $config{char} );
            Log::message( "Closing log [$log].\n" );
            my $fp = $log_files->{$file};
            close $fp;
            delete $log_files->{$file};
            rotate_log( $log );
        }
    }
}

sub rotate_log {
    my ( $file ) = @_;

    return if !-f $file;

    # Get a list of files to rotate.
    my $to_rotate = [ [ $file => "$file.1" ] ];
    for ( my $i = 1 ; $i < 30 ; $i++ ) {
        if ( -f "$file.$i" ) {
            push @$to_rotate, [ "$file.$i" => "$file." . ( $i + 1 ) ];
            next;
        }
        if ( -f "$file.$i.bz2" ) {
            push @$to_rotate, [ "$file.$i.bz2" => "$file." . ( $i + 1 ) . '.bz2' ];
            next;
        }
        last;
    }

    # Rotate in reverse order, to avoid accidentally overwriting anything.
    foreach ( reverse @$to_rotate ) {
        my ( $from, $to ) = @$_;
        Log::message( "Rotating log [$from] => [$to].\n" );
        rename $from => $to;
    }
}

sub get_map_id {
    my ( $map_name ) = @_;
    $map_name ||= $field->baseName;
    our $map_name_cache ||= {};
    return $map_name_cache->{$map_name} if $map_name_cache->{$map_name};
    dbh->do( 'insert into iro_map (map_name) values (?) on duplicate key update map_id=last_insert_id(map_id)', undef, $map_name );
    return $map_name_cache->{$map_name} = dbh->{mysql_insertid};
}

sub dumpBuyer {
    my ( $buyer ) = @_;

    $csv->combine(    #
        $buyer->{id},
        $buyer->{name}  || '',
        $buyer->{party} || '',
        $buyer->{guild} || '',
        $buyer->{field},
        $buyer->{pos}->{x},
        $buyer->{pos}->{y},
        $buyer->{title},
    );

    logit( 'buyers', $csv->string . "\n" );
}

sub dumpPlayer {
    my ( $player ) = @_;

    $csv->combine(
        $player->{id},
        $player->{name}  || '',
        $player->{job}   || '',
        $player->{lv}    || '',
        $player->{party} || '',
        $player->{guild} || '',
        scalar localtime( $player->{time_start} ),
        sprintf( '%.2f', $player->{time_end} - $player->{time_start} ),
        $player->{field},
        Utils::distance( $player->{pos_start}, $player->{pos_end} ),
        $player->{pos_start}->{x},
        $player->{pos_start}->{y},
        $player->{pos_end}->{x},
        $player->{pos_end}->{y},
    );

    logit( 'players', $csv->string . "\n" );

    if ( $config{logPlayersInLockMap} && $field->baseName eq $config{lockMap} && $char->{party} && !$char->{party}{users}{ pack 'V', $player->{id} } ) {
        my $dt = sprintf '%.2f', $player->{time_end} - $player->{time_start};
        my $dp = int Utils::distance( $player->{pos_start}, $player->{pos_end} );
        eval { OpenKore::Plugin::Spread::send_log( { switch => 'notice', message => "Saw player [$player->{name}] for [$dt] seconds, over [$dp] tiles." } ); };
    }
}

sub formatMonster {
    my ( $mob ) = @_;

    $csv->combine(
        $mob->{id},
        $mob->{type}    || '',
        $mob->{name}    || '',
        $mob->{dmgFrom} || '',
        $mob->{dmgTo}   || '',
        scalar localtime( $mob->{time_start} ),
        sprintf( '%.2f', $mob->{time_end} - $mob->{time_start} ),
        $mob->{field},
        sprintf( '%.2f', Utils::distance( $mob->{pos_start}, $mob->{pos_end} ) ),
        $mob->{pos_start}->{x},
        $mob->{pos_start}->{y},
        $mob->{pos_end}->{x},
        $mob->{pos_end}->{y},
    );

    return $csv->string;
}
sub dumpMonster { logit( 'monsters', formatMonster( @_ ) . "\n" ); }

sub formatMVP {
    my ( $mvp ) = @_;

    $csv->combine(
        $mvp->{mvp_name}      || '',
        $mvp->{killer}        || '',
        $mvp->{time_of_death} || '',
        $mvp->{pos}->{x}      || '',
        $mvp->{pos}->{y}      || '',
        $mvp->{pos}->{map}    || '',
        scalar localtime(),
    );

    return $csv->string;
}
sub dumpMVP { logit( 'mvp', formatMVP( @_ ) . "\n" ); }

sub log_event {
    my ( $event, $startstop, @args ) = @_;

    # This can be called before we're fully logged in.
    return if !$char;

    logit_csv( events => [ time, $char->{name}, $event, $startstop, @args ] );
}

sub logit_csv {
    my ( $file, $msg ) = @_;
    $csv->combine( @$msg );
    logit( $file, $csv->string . "\n" );
}

sub logit {
    my ( $file, $msg ) = @_;
    my $fp = $log_files->{$file};
    if ( !$fp ) {
        my $log = File::Spec->catfile( $Settings::logs_folder, sprintf '%s_%s_%s.txt', $file, $config{username}, $config{char} );
        open $fp, '>>', $log;
        select( ( select( $fp ), $|++ )[0] );
        $log_files->{$file} = $fp;
    }
    print $fp $msg;
}

sub dbh {
    our $dbh;
    if ( !$dbh ) {
        $dbh = DBI->connect( 'dbi:mysql:ragnarok;host=10.1.0.1', 'ragnarok', 'ragnarok' );
        $dbh->{mysql_auto_reconnect} = 1;
    }
    return $dbh;
}

1;
