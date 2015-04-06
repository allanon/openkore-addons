package OpenKore::Plugin::Spread;

=head1 NAME

spread - a plugin for openkore which provides spread-based messaging.

=head1 VERSION

Version 0.1

=head1 DESCRIPTION

The list-vendors plugin which allows sending messages over spread instead 
of the RO chat system.

http://www.spread.org/download.html

=head1 USAGE

=head2 commands

=over

=item sp_pm recipient message

Send private message to recipient. If recipient's name contains spaces, 
those spaces must be replaced with underscores. For example, to send 
"hello" to Miriam Thorold, do:

sp_pm "Miriam Thorold" hello

=over

=head1 AVAILABILITY

C<Ask Author>

=head1 COPYRIGHT

This source code is licensed under the GNU General Public License, Version 2.
See L<http://www.gnu.org/licenses/gpl.html>

=head1 AUTHOR

Allanon <kore at allanon dot elfhame dot net>

=cut

use strict;
use Plugins;
use Globals;
use Utils;
use Misc;
use Log qw(message warning error);
use AI;
use Time::HiRes;
use Translation;

use Spread;
use JSON::XS;

our $debug        = 0;
our $bot_group    = 'bot';
our $log_group    = 'bot_log';
our $item_monitor = {};
our $last_attack  = {};
our $followers    = {};
our $far_follower = {};
our $fieldIsCity  = {};
our $bot_party   ||= {};
our $rolling_log ||= [];
our $spread_mbox;
our $spread_msg = 0;

our $MSG_PRIVMSG      = 1;
our $MSG_PARTYMSG     = 2;
our $MSG_GUILDMSG     = 3;
our $MSG_PARTY_UPDATE = 4;
our $MSG_ADMIN        = 5;
our $MSG_LOGGING      = 100;
our $MSG_STATUS_REQ   = 200;
our $MSG_STATUS_RES   = 201;
our $MSG_KILLS_REQ    = 202;
our $MSG_KILLS_RES    = 203;
our $MSG_DEATHS_REQ   = 204;
our $MSG_DEATHS_RES   = 205;
our $MSG_SHOP_REQ     = 206;
our $MSG_SHOP_RES     = 207;
our $MSG_ONLINE_REQ   = 208;
our $MSG_ONLINE_RES   = 209;

our $config_defaults = {
    spread_followerMoveDist   => 30,
    spread_followerMoveAmount => 0.25,
    spread_followerWaitDist   => 10,
    spread_followerWaitTime   => 1,
    spread_groups             => '',
};

Plugins::register( 'spread', 'provides spread messaging', \&Unload, \&Unload );

my $hooks = Plugins::addHooks(
    [ start3                        => \&checkConfig ],
    [ mainLoop_pre                  => \&checkMessages ],
    [ Command_post                  => \&onCommand ],
    [ exp_gained                    => \&expGainedLogger ],
    [ base_level                    => \&baseLevelLogger ],
    [ job_level                     => \&jobLevelLogger ],
    [ item_gathered                 => \&onPacketItem ],
    [ packet_item_removed           => \&onPacketItem ],
    [ 'packet/stat_info'            => \&onPacketStatInfo ],
    [ start3                        => \&onStart3 ],
    [ shutdown                      => \&onShutdown ],
    [ self_died                     => \&onSelfDied ],
    [ 'packet/inventory_item_added' => \&itemLogger ],
    [ 'packet_pre/shop_sold'        => \&onShopSold ],
    [ packet_privMsg                => \&onPacketPrivateMessage ],
    [ packet_partyMsg               => \&onPacketPartyMessage ],
    [ packet_pubMsg                 => \&onPacketPublicMessage ],
    [ packet_guildMsg               => \&onPacketGuildMessage ],
    [ packet_attack                 => \&onPacketAttack ],
    [ target_died                   => \&onTargetDied ],
    [ checkSelfCondition            => \&onCheckSelfCondition ],
    [ packet_partyJoin              => \&onPacketPartyJoin ],
    [ in_game                       => \&onInGame ],
    [ 'packet_pre/errors'           => \&onErrors ],
);

# spread_followers 1                      - true if there is at least one bot following this bot
# spread_followersOnline 1                - true if there is at least one online bot following this bot
# spread_followersInclude name1,name2,... - true if any of name1, name2, etc are following this bot
# spread_leaderInParty 1                  - true if the bot's followTarget is the same party (or bot party)
# spread_leaderInTown 1                   - true if the bot's followTarget is in a town
# spread_leaderNotInTown 1                - true if the bot's followTarget is not in a town
# spread_playerNearby name1,name2,...     - true if any of name1, name2, etc are nearby

sub onCheckSelfCondition {
    my ( undef, $args ) = @_;

    return if !$args->{return};

    my $prefix = "$args->{prefix}_spread";

    return $args->{return} = 0 if $config{"${prefix}_followers"} && !keys %$followers;
    return $args->{return} = 0 if $config{"${prefix}_followersOnline"} && !followersOnline();
    return $args->{return} = 0 if $config{"${prefix}_followersInclude"} && !grep { $followers->{$_} } split /\s*,\s*/, $config{"${prefix}_followersInclude"};
    return $args->{return} = 0 if $config{"${prefix}_inTown"} && $field->isCity;
    if ( $config{"${prefix}_leaderInParty"} ) {
        return $args->{return} = 0 if !$config{follow} || !$config{followTarget};
        my ( $leader ) = grep { $_->{name} eq $config{followTarget} } values %{ $char->{party}->{users} };
        return $args->{return} = 0 if !$leader;
    }
    if ( $config{"${prefix}_leaderInTown"} ) {
        return $args->{return} = 0 if !$config{follow} || !$config{followTarget};
        my ( $leader ) = grep { $_->{name} eq $config{followTarget} } values %{ $char->{party}->{users} };
        return $args->{return} = 0 if $leader && !fieldIsCity( $leader->{map} );
    }
    if ( $config{"${prefix}_leaderNotInTown"} ) {
        return $args->{return} = 0 if !$config{follow} || !$config{followTarget};
        my ( $leader ) = grep { $_->{name} eq $config{followTarget} } values %{ $char->{party}->{users} };
        return $args->{return} = 0 if $leader && fieldIsCity( $leader->{map} );
    }
    if ( $config{"${prefix}_playerNearby"} ) {
        my $names = { map { ( $_ => 1 ) } split /\s*,\s*/, $config{"${prefix}_playerNearby"} };
        my ( $found ) = grep { $names->{ $_->{name} } } @{ $playersList->getItems || [] };
        return $args->{return} = 0 if !$found;
    }
}

sub followersOnline {
    return scalar grep { $_->{online} } values %$followers;
}

sub fieldIsCity {
    my ( $fieldName ) = @_;
    return if !$fieldName;
    $fieldName =~ s/\.gat$//;
    $fieldIsCity->{$fieldName} = $field->isCity if !defined $fieldIsCity->{$fieldName} && $fieldName eq $field->baseName;
    if ( !defined $fieldIsCity->{$fieldName} ) {
        eval {
            my $f = Field->new( name => $fieldName );
            $fieldIsCity->{$fieldName} = $f->isCity;
        };
    }
    return $fieldIsCity->{$fieldName};
}

sub onShopSold {
    my ( undef, $args ) = @_;

    my $i      = $args->{number};
    my $amount = $args->{amount};
    my $earned = $amount * $articles[$i]{price};
    my $msg = sprintf "Sold: %sx %s %sz (%s left)", $amount, $articles[$i]{name}, commify( $earned ), commify( $articles[$i]{quantity} - $amount );
    send_log( { switch => 'notice', message => $msg } );
}

sub commify {
    my ( $num ) = @_;
    $num =~ s/(^[-+]?\d+?(?=(?>(?:\d{3})+)(?!\d))|\G\d{3}(?=\d))/$1,/g;
    return $num;
}

sub onPacketItem {
    spreadNotice('item');
}

sub onPacketStatInfo {
    spreadNotice('stat_info');
}

sub onStart3 {
    spreadNotice('start');
    $config{spreadServer} ||= '';
}

sub onShutdown {
    spreadNotice('quit');
}

sub onSelfDied {
    spreadNotice( 'death' );
    rolling_log( 'died', $last_attack->{source}->{name} );
    send_log(
        {
            switch => 'death',
            killer => $last_attack->{source}->{name},
            nearby => scalar( @{ $monstersList->getItems } ),
            level  => $char->{lv},
            weight => sprintf( '%.1f', 100 * $char->{weight} / $char->{weight_max} ),
            map    => $field->baseName,
            x      => $char->{pos}->{x},
            y      => $char->{pos}->{y},
        }
    );
}

sub onInGame {
    send_spread( $bot_group, $MSG_ADMIN, { switch => 'send_full_party_packet' } );
}

sub onErrors {
    my ( undef, $args ) = @_;
    return if $args->{type} != 2;
    send_log( { switch => 'notice', message => 'Critical Error: Dual login prohibited - Someone trying to login!' } );
}

sub onPacketPartyJoin {
    # Our party info just got nuked. Re-add it.
    foreach my $ID ( keys %$bot_party ) {
        party_packet( $bot_party->{$ID} );
    }
}

sub onPacketPrivateMessage {
    my ( undef, $args ) = @_;
    spreadNotice( 'private_message', $args );
    send_log(
        {
            switch  => 'pm',
            from    => $args->{privMsgUser} || 'unknown',
            message => $args->{privMsg},
            map     => $field->baseName,
            x       => $char->{pos}->{x},
            y       => $char->{pos}->{y},
        }
    ) if !$spread_msg;
}

sub onPacketPartyMessage {
    my ( undef, $args ) = @_;
    spreadNotice( 'party_message', $args );
    send_log(
        {
            switch  => 'p',
            from    => $args->{MsgUser} || 'unknown',
            message => $args->{Msg},
            map     => $field->baseName,
            x       => $char->{pos}->{x},
            y       => $char->{pos}->{y},
        }
    ) if !$spread_msg;
}

sub onPacketPublicMessage {
    my ( undef, $args ) = @_;
    return if $config{ignore_match} && $args->{Msg} =~ /$config{ignore_match}/;
    spreadNotice( 'public_message', $args );
    send_log(
        {
            switch  => 'c',
            from    => $args->{MsgUser} || 'unknown',
            message => $args->{Msg},
            map     => $field->baseName,
            x       => $char->{pos}->{x},
            y       => $char->{pos}->{y},
        }
    ) if !$spread_msg;
}

sub onPacketGuildMessage {
    my ( undef, $args ) = @_;
    return if $config{ignore_match} && $args->{Msg} =~ /$config{ignore_match}/;
    spreadNotice( 'guild_message', $args );
    send_log(
        {
            switch  => 'g',
            from    => $args->{MsgUser} || 'unknown',
            message => $args->{Msg},
            map     => $field->baseName,
            x       => $char->{pos}->{x},
            y       => $char->{pos}->{y},
        }
    ) if !$spread_msg;
}

sub onPacketAttack {
    my ( $name, $args ) = @_;

    return if $args->{dmg} <= 0;

    if ( $args->{sourceID} eq $accountID ) {
        my $target = Actor::get( $args->{targetID} );
        return if !$target;
        rolling_log( 'dmg_dealt', $target->name, $args->{dmg} );
    } elsif ( $args->{targetID} eq $accountID ) {
        my $source = Actor::get( $args->{sourceID} );
        return if !$source;

        $last_attack = {
            source => $source,
            time   => time,
            damage => $args->{dmg},
        };

        rolling_log( 'dmg_taken', $source->name, $args->{dmg} );
    }
}

sub onTargetDied {
    my ($name, $args) = @_;

	return if !$args->{monster};

    rolling_log( 'kill', $args->{monster}->name, 1 );
}

sub spreadNotice {
    my ($type, $args) = @_;
    for ( my $i = 0 ; exists $config{"spreadNotice_$i"} ; $i++ ) {
        my $prefix = "spreadNotice_$i";
        my $match  = 1;
        my $msg    = '';

        # We normally want notices even in manual AI.
        $config{"${prefix}_manualAI"} = 2 if !defined $config{"${prefix}_manualAI"};

        # Check standard self conditions like hp and weight.
        if ( $char && !Misc::checkSelfCondition($prefix) ) {
            $match = 0;
        }

        if ( $char && $config{"${prefix}_item"} && $type ne 'stat_info' ) {
            my $str = $config{"${prefix}_item"};
            my ( $name, $range ) = $str =~ /(.*?)\s+([<>=].*)/o;
            my $item = $char->inventory->getByName($name);
            my $amount = $item ? $item->{amount} : 0;
            if ( inRange( $amount, $range ) ) {
                $msg = sprintf( 'has %s x %d', $name, $amount );
            } else {
                $match = 0;
            }
        } elsif ( $config{"${prefix}_item"} ) {
            $match = 0;
        }

        if ( $char && $config{"${prefix}_weight"} ) {
            my $range  = $config{"${prefix}_weight"};
            my $amount = $char->{weight};
            $amount = 100 * $amount / ($char->{weight_max} || 1) if $range =~ s/%$//;
            if ( inRange( $amount, $range ) ) {
#                $msg = sprintf( 'weight is %.1f%%', 100 * $char->{weight} / $char->{weight_max} );
            } else {
                $match = 0;
            }
        } elsif ( $config{"${prefix}_weight"} ) {
            $match = 0;
        }

        if ( $char && $config{"${prefix}_pm"} && $type eq 'private_message' && !$overallAuth{$args->{MsgUser}} ) {
            $msg = sprintf( '[PM] [%s]: %s', $args->{MsgUser}, $args->{Msg} );
            $config{"${prefix}_NOMATCH"} = 1;
        } elsif ( $config{"${prefix}_pm"} ) {
            $match = 0;
        }

        # This has to work even if $char isn't defined. Be sure not to 
        # auto-vivify $char.
        if ( $config{"${prefix}_status"} ) {
            if ( $char && $config{"${prefix}_status"} =~ /\bexit\b/ && $type eq 'quit' ) {
                $msg = 'exiting';
            } elsif ( $config{"${prefix}_status"} =~ /\bdead\b/ && ( $type eq 'death' || ( $char && $char->{dead} ) ) ) {
                my $killed_by = 'unknown';
                if ( $last_attack->{source} ) {
                    $killed_by = $last_attack->{source}->name;
                }
                my $location = sprintf(
                    '%s %d %d', $field ? $field->name : 'unknown', $char
                    ? ( $char->{pos}->{x}, $char->{pos}->{y} )
                    : ( '?', '?' ),
                );
                my $monsters = {};
                foreach my $monster ( @{ $monstersList->getItems || [] } ) {
                    next if !$char;
                    $monsters->{ $monster->name }++;
                }
                $monsters
                    = %$monsters
                    ? join(
                    ', ', map {"$monsters->{$_}x $_"}
                        sort { $monsters->{$b} <=> $monsters->{$a} }
                        keys %$monsters
                    )
                    : 'no monsters';
                $msg = "died (killed by $killed_by, at $location) ($monsters nearby)";
            } else {
                $match = 0;
            }
        }

        if ( $match && $msg && $config{"${prefix}_NOMATCH"} ) {
            send_log(
                {
                    switch  => 'notice',
                    message => $msg,
                }
            );
        }

        $config{"${prefix}_NOMATCH"} = !$match;
    }
}

sub Unload {
    Plugins::delHooks( $hooks );
    if ( $spread_mbox ) {
        message "spread: Disconnecting.\n";
        Spread::disconnect( $spread_mbox );
        $spread_mbox = undef;
    }
    message "spread unloaded.\n";
}

# checks configuration for silly settings
sub checkConfig {
    foreach ( sort keys %$config_defaults ) {
        next if exists $config{$_};
        configModify( $_ => $config_defaults->{$_} );
    }
}

# log item gathering
sub itemLogger {
    my ( undef, $args ) = @_;
    return if $args->{fail};
    my $item = $char->inventory->getByServerIndex( $args->{index} );
    message "itemLogger2: item missing\n" if !$item;
    return                                if !$item;
    send_log(
        {
            switch     => 'gather',
            amount     => $args->{amount},
            cards      => [ unpack 'vvvv', $args->{cards} ],
            name       => $item->{name},
            item_id    => $args->{nameID},
            type       => $args->{type},
            type_equip => $args->{type_equip},
            upgrade    => $args->{upgrade},
            map        => $field->baseName,
            x          => $char->{pos}->{x},
            y          => $char->{pos}->{y},
        }
    );
    rolling_log( 'item', $args->{name}, $args->{amount} );
}

# log exp gained
sub expGainedLogger {
    my ( undef, $param ) = @_;
    return if !$char || !$char->{lv};
    send_log(
        {
            switch     => 'exp_gained',
            level      => $char->{lv},
            job_level  => $char->{lv_job},
            xp         => $char->{exp},
            xp_max     => $char->{exp_max},
            jp         => $char->{exp_job},
            jp_max     => $char->{exp_job_max},
        }
    );
}

# log level up
sub baseLevelLogger {
    my ( undef, $param ) = @_;
    return if !$char || !$char->{lv};
    return if $param->{name} ne 'You';
    send_log(
        {
            switch     => 'base_level',
            level      => $char->{lv},
        }
    );
}

# log job level up
sub jobLevelLogger {
    my ( undef, $param ) = @_;
    return if !$char || !$char->{lv_job};
    return if $param->{name} ne 'You';
    send_log(
        {
            switch     => 'job_level',
            job_level  => $char->{lv_job},
        }
    );
}

sub send_log {
    my ($msg) = @_;
    send_spread( $log_group, $MSG_LOGGING, $msg );
}

sub send_spread {
    my ($group, $type, $msg) = @_;
    $msg->{char} ||= my_name();
    Log::debug( TF( "[spread] send [%s]\n", $type ), 'spread' );
    Spread::multicast( mbox(), Spread::FIFO_MESS, $group, $type, encode_json($msg) );
}

sub party_quest_complete {
    my ( @quest_ids ) = @_;
    my $counts = {};
    foreach my $bot ( values %$bot_party ) {
        next if !$char || !$char->{party};
        next if $bot->{party} ne $char->{party}->{name};
        my @todo = @quest_ids ? @quest_ids : keys %{ $bot->{quest_counts} || {} };
        foreach my $quest_id ( @todo ) {
            next if !$bot->{quest_counts}->{$quest_id};
            return 0;
        }
    }
    return 1;
}

# dispatches plugin commands
sub onCommand {
    my ( undef, $param ) = @_;
    if ( $param->{switch} eq 'sp_quest' ) {
        my $quests = { map { $_ => 1 } map { keys %{ $_->{quest_counts} || {} } } values %$bot_party };
        my ( $switch, @quest_ids ) = parseArgs( $param->{input} );
        if ( @quest_ids ) {
            my $keep_ids = { map { $_ => 1 } @quest_ids };
            delete $quests->{$_} foreach grep { !$keep_ids->{$_} } keys %$quests;
        }
        my $fmt = sprintf '@%s @%s @%s @%s', ( '>' x 7 ), ( '<' x 28 ), ( '>' x 7 ), ( '<' x 34 );
        my $msg;
        $msg .= center( " " . T( "Quest List" ) . " ", 79, '-' ) . "\n";
        $msg .= swrite( $fmt, [ 'Quest ID', 'Title', 'Count', 'Character' ] );
        foreach my $quest_id ( sort { $a <=> $b } keys %$quests ) {
            my $title = $quests_lut{$quest_id}{title} || '';
            my @missions = map { [ $_->{quest_counts}->{$quest_id}, $_->{char} ] }
                grep { $_->{quest_counts}->{$quest_id} } sort { $a->{char} cmp $b->{char} } values %$bot_party;
            my $extra = shift( @missions ) || [];
            $msg .= swrite( $fmt, [ $quest_id, $quests_lut{$quest_id}{title} || '', @$extra ] );
            $msg .= swrite( $fmt, [ '', '', @$_ ] ) foreach @missions;
        }
        $msg .= sprintf "%s\n", ( '-' x 79 );
        Log::message( $msg, 'list' );
        $param->{return} = 1;
    } elsif ( $param->{switch} eq 'sp_pm' ) {
        my $name = my_name();
        my ( $switch, $to, $msg ) = parseArgs( $param->{input}, 3 );
        priv_msg( $to, $msg );
        $param->{return} = 1;
    } elsif ( $param->{switch} eq 'sp_p' ) {
        send_spread(
            $bot_group,
            $MSG_PARTYMSG,
            {
                party   => my_party(),
                message => substr( $param->{input}, 5 ),
            }
        );
        $param->{return} = 1;
    } elsif ( $param->{switch} eq 'sp_g' ) {
        send_spread(
            $bot_group,
            $MSG_GUILDMSG,
            {
                guild   => my_guild(),
                message => substr( $param->{input}, 5 ),
            }
        );
        $param->{return} = 1;
    } elsif ( $param->{switch} eq 'sp_c' ) {
        send_spread(
            $bot_group,
            $MSG_PRIVMSG,
            {
                to      => 'ALL',
                message => substr( $param->{input}, 5 ),
            }
        );
        $param->{return} = 1;
    } elsif ( $param->{switch} eq 'sp_notify' ) {
        send_log( { switch => 'notice', message => substr $param->{input}, 10 } );
        $param->{return} = 1;
    } elsif ( $param->{switch} eq 'sp_party' ) {
        show_party();
        $param->{return} = 1;
    }

    if ( $param->{switch} eq 'sp_pl' ) {
        open FP, '>/tmp/player_list.txt';
        print FP YAML::Dump( $playersList->getItems );
        close FP;
    }
}

sub show_party {
    Log::message( sprintf "%-16.16s %-10.10s %6s %s %-12s %3s,%-3s %3s %3s %s\n",
        'Name', 'Party', 'Level', 'Flg', 'Map', 'X', 'Y', 'HP%', 'SP%', );
    foreach ( sort { $a->{char} cmp $b->{char} } values %$bot_party ) {
        my $flags = join '',
            ( $_->{online}     ? 'O' : ' ' ),
            ( $_->{partyadmin} ? 'A' : ' ' ),
            ( $_->{follow}     ? 'F' : ' ' );
        my $msg = sprintf "%-16.16s %-10.10s %3s/%2s %s %-12s %3d,%-3d %3d %3d\n",
            $_->{char},
            $_->{party},
            $_->{lv},
            $_->{lv_job},
            $flags,
            ( split /\.gat/, $_->{map} )[0],
            $_->{x},
            $_->{y},
            100 * $_->{hp} / ( $_->{hp_max} || 1 ),
            100 * $_->{sp} / ( $_->{sp_max} || 1 );
        if ( time - $_->{last_msg_stamp} > 60 ) {
            Log::warning( $msg );
        } else {
            Log::message( $msg );
        }
    }
}

sub priv_msg {
    my ( $to, $msg ) = @_;
    send_spread(
        $bot_group,
        $MSG_PRIVMSG,
        {
            to      => $to,
            message => $msg,
        }
    );
    return 1;
}

sub my_name {
    return '' if !defined $config{char};
    return '' if !$chars[ $config{char} ];
    return $chars[ $config{char} ]->{name} || '';
}

sub my_party {
    return 'NONE' if !$char;
    return 'NONE' if !$char->{party};
    return $char->{party}->{name} || '';
}

sub my_guild {
    return $guild{name} || '';
}

sub msg_to_me {
    my ( $to ) = @_;
    return 1 if $to eq my_name();
    return 1 if $to eq 'ALL';
    return 1 if $to eq 'ONLINE' && $net->getState == Network::IN_GAME;
    return 1 if $to eq 'OFFLINE' && $net->getState != Network::IN_GAME;
    return 1 if $to eq 'LEADERS' && !$config{follow};
    return 1 if $to eq 'FOLLOWERS' && $config{follow};
    return 1 if existsInList( $config{spread_groups}, $to );
    return 1 if $to =~ /^(\w+)=(.*)$/ && $config{$1} eq $2;
    return;
}

sub checkMessages {
    return if !$char;
    return if !$field;

    my $mbox = mbox();
    return if !$mbox;

    # Let the message parsers know that we might be sending an internal message.
    # Otherwise, they'll re-broadcast them as notifications.
    $spread_msg = 1;

    while ( 1 ) {
        my ( $message_size ) = Spread::poll( $mbox );
        last if !$message_size;

        my ( $service_type, $sender, $groups, $mess_type, $endian, $message ) = Spread::receive( $mbox, 0.03 );
        last if !defined $message;

        my $msg = eval { decode_json( $message ); };
        next if !$msg;

        Log::debug( TF( "[spread] receive [%s] from [%s]\n", $mess_type, $msg->{char} ), 'spread' );

        if ( $msg->{ID} && $accountID ne pack 'V', $msg->{ID} ) {
            my $bot = $bot_party->{ pack 'V', $msg->{ID} } ||= { ID => $msg->{ID} };
            $bot->{last_msg_stamp} = Time::HiRes::time;
        }

        my $packetParser = $::packetParser || Network::Receive->create( $::config{serverType} );
        if ( $mess_type == $MSG_PRIVMSG ) {
            next if !msg_to_me( $msg->{to} );
            $packetParser->private_message( { privMsgUser => $msg->{char}, privMsg => $msg->{message} } );
            if ( $msg->{message} eq 'available to deal?' && !%incomingDeal && !%outgoingDeal && !%currentDeal ) {
                foreach my $pl ( @{ $playersList->getItems() } ) {
                    Commands::cmdDeal( undef, $pl->{binID} ) if $pl->name eq $msg->{char};
                }
            }
            my @parts = split /\s+/, $msg->{message}, 2;
            if ( $Commands::handlers{ $parts[0] } || $parts[0] =~ /^(macro)$/o ) {
                Commands::run( $msg->{message} );
            }
            Commands::run( "$1" ) if $msg->{message} =~ /^do\s+(.*)$/o;
            Plugins::callHook(
                spread_pm => {
                    privMsgUser => $msg->{char},
                    privMsg     => $msg->{message},
                }
            );
        } elsif ( $mess_type == $MSG_PARTYMSG ) {
            next if $msg->{party} ne my_party();
            if ( $msg->{message} =~ /^([A-Z_]+)\s+(.*)$/o ) {
                ( $msg->{to}, $msg->{message} ) = ( $1, $2 );
                next if !msg_to_me( $msg->{to} );
            }
            $packetParser->party_chat( { message => "$msg->{char} : $msg->{message}" } );
            my @parts = split /\s+/, $msg->{message}, 2;
            if ( $Commands::handlers{ $parts[0] } || $parts[0] =~ /^(macro)$/o ) {
                Commands::run( $msg->{message} );
            }
            Commands::run( "$1" ) if $msg->{message} =~ /^do\s+(.*)$/o;
        } elsif ( $mess_type == $MSG_GUILDMSG ) {
            next if $msg->{guild} ne my_guild();
            $packetParser->guild_chat( { message => "$msg->{char} : $msg->{message}" } );
        } elsif ( $mess_type == $MSG_PARTY_UPDATE ) {
            party_packet( $msg );
        } elsif ( $mess_type == $MSG_STATUS_REQ ) {
            next if !msg_to_me( $msg->{to} );
            send_status( $msg->{reply_group} || $log_group );
        } elsif ( $mess_type == $MSG_KILLS_REQ ) {
            next if !msg_to_me( $msg->{to} );
            send_log_summary( $msg->{reply_group} || $log_group, $MSG_KILLS_RES, 'kill' );
        } elsif ( $mess_type == $MSG_DEATHS_REQ ) {
            next if !msg_to_me( $msg->{to} );
            send_log_summary( $msg->{reply_group} || $log_group, $MSG_DEATHS_RES, 'died' );
        } elsif ( $mess_type == $MSG_SHOP_REQ ) {
            next if !msg_to_me( $msg->{to} );
            send_shop( $msg->{reply_group} || $log_group, $MSG_SHOP_RES );
        } elsif ( $mess_type == $MSG_ONLINE_REQ ) {
            next if !msg_to_me( $msg->{to} );
            send_online( $msg->{reply_group} || $log_group );
        } elsif ( $mess_type == $MSG_ADMIN ) {
            if ( $msg->{switch} eq 'send_full_party_packet' ) {
                our $last_full_update = 0;
                # message "SPREAD: Sending full party packet, as requested by $msg->{char}.\n";
            }
        } elsif ( $mess_type >= $MSG_LOGGING ) {

            # Ignore logging message.
        } elsif ( $mess_type == 0 ) {

            # Ignore administrative message.
        } elsif ( $mess_type ) {
            message "SPREAD: (From: $sender) : $message\n";
        }
    }

    # TODO: Do this with a timeout.
    our $last_update ||= 0;
    if ( Time::HiRes::time - $last_update >= 0.2 ) {
        send_party_update();
        $last_update = Time::HiRes::time;
    }

    # All done with internal messaging.
    $spread_msg = 0;
}

sub quest_counts {
    return {
        map {
            $_ => List::Util::max( map { $_->{goal} - $_->{count} } grep { defined $_->{count} } values %{ $questList->{$_}->{missions} } )
            } grep {
            $questList->{$_}->{missions} && keys %{ $questList->{$_}->{missions} }
            } keys %$questList
    };
}

sub format_quests {
    return [
        map {
            my $title    = ${ $quests_lut{$_} || {} }{title};
            my $quest    = $questList->{$_};
            my $mobs     = $quest->{missions} || {};
            my $missions = [
                map {
                    {
                        mob_id   => $_,
                        mob_name => $mobs->{$_}->{mobName},
                        count    => $mobs->{$_}->{count},
                        goal     => $mobs->{$_}->{goal},
                    }
                    }
                    sort keys %$mobs
            ];
            {
                quest_id => $_,
                title    => $title || "Unknown #$_",
                active   => $quest->{active},
                timeout  => $quest->{time},
                missions => $missions,
            }
            } sort {
            $a <=> $b
            } keys %$questList
    ];
}

sub send_status {
    my ( $group ) = @_;

    return if !$char;

    my $inventory = [
        map {
            {
                name     => $_->{name},
                amount   => $_->{amount},
                type     => $itemTypes_lut{ $_->{type} },
                equipped => $_->{equipped},
                ident    => $_->{identified},
            }
            } @{ $char->inventory->getItems }
    ];

    my $totals = {};
    foreach ( @$rolling_log ) {
        $totals->{ $_->{type} } += $_->{amount};
    }

    my $ai_mode = $AI == AI::AUTO ? 'auto' : $AI == AI::MANUAL ? 'manual' : $AI == AI::OFF ? 'off' : 'unknown';
    my ($ai_action, $ai_target);
    my ( $i );
    if ( '' ne ( $i = AI::findAction( 'attack' ) ) ) {
        my $args = AI::args( $i );
        my $actor = $args ? Actor::get( $args->{ID} ) : undef;
        $ai_action = 'attack';
        $ai_target = $actor ? $actor->name : 'unknown';
    } elsif ( '' ne ( $i = AI::findAction ('follow') ) ) {
        my $args = AI::args ($i);
        $ai_action = 'follow';
        $ai_target = $args->{following} || $args->{ai_follow_lost} ? $args->{name} : 'unknown';
	} elsif ( '' ne ( $i = Utils::DataStructures::binFindReverse( \@AI::ai_seq, 'route' ) ) ) {
        my $args = AI::args( $i );
        $ai_action = 'route';
        if ( $args->{dest}{map} eq $field->baseName ) {
            $ai_target = "($args->{dest}{pos}{x}, $args->{dest}{pos}{y})";
        } elsif ( !defined $args->{dest}{pos}{x} ) {
            $ai_target = $args->{dest}{map};
        } else {
            $ai_target = "$args->{dest}{map} ($args->{dest}{pos}{x}, $args->{dest}{pos}{y})";
        }
    } elsif ( '' ne ( $i = AI::findAction ('macro') ) ) {
	    $ai_action = 'macro';
	    $ai_target = '';
	} else {
	    $ai_action = 'unknown';
	    $ai_target = '';
	}

    send_spread(
        $group,
        $MSG_STATUS_RES, {
            id         => unpack( 'V', $accountID ),
            hp         => $char->{hp},
            hp_max     => $char->{hp_max},
            map        => $field->baseName,
            pos        => calcPosition( $char ),
            lockMap    => $config{lockMap},
            sp         => $char->{sp},
            sp_max     => $char->{sp_max},
            lv         => $char->{lv},
            xp         => $char->{exp},
            xp_max     => $char->{exp_max},
            lv_job     => $char->{lv_job},
            jp         => $char->{exp_job},
            jp_max     => $char->{exp_job_max},
            zeny       => $char->{zeny},
            weight     => int $char->{weight},
            weight_max => $char->{weight_max},
            inventory  => $inventory,
            quests     => format_quests(),
            quest_counts => quest_counts(),
            job        => $jobs_lut{ $char->{jobID} },
            totals     => $totals,
            ai_mode    => $ai_mode,
            ai_action  => $ai_action,
            ai_target  => $ai_target,

            party  => my_party(),
            deaths => [ grep { $_->{type} eq 'died' } @$rolling_log ],

            partyadmin => $char->{party}->{users}->{$accountID}->{admin} ? 1                     : 0,
            online     => $net->getState == Network::IN_GAME             ? 1                     : 0,
            follow     => $config{follow}                                ? $config{followTarget} : '',

            # Conditional archer stuff.
            arrows => ( $char->{arrow} && ( $char->inventory->getByServerIndex( $char->{arrow} ) || {} )->{amount} || 0 ),
            arrows_max => ( $config{autoArrowsMaxArrows} || 1 ),

            # Conditional merchant stuff.
            cart_weight     => $cart{exists} ? int $cart{weight}     : 0,
            cart_weight_max => $cart{exists} ? int $cart{weight_max} : 0,
            homunculus      => {
                name   => $char->{homunculus} ? $char->{homunculus}->{name}               : 0,
                job    => $char->{homunculus} ? $jobs_lut{ $char->{homunculus}->{jobId} } : 0,
                hp     => $char->{homunculus} ? $char->{homunculus}->{hp}                 : 0,
                hp_max => $char->{homunculus} ? $char->{homunculus}->{hp_max}             : 0,
                sp     => $char->{homunculus} ? $char->{homunculus}->{sp}                 : 0,
                sp_max => $char->{homunculus} ? $char->{homunculus}->{sp_max}             : 0,
                lv     => $char->{homunculus} ? $char->{homunculus}->{lv}                 : 0,
                xp     => $char->{homunculus} ? $char->{homunculus}->{exp}                : 0,
                xp_max => $char->{homunculus} ? $char->{homunculus}->{exp_max}            : 0,
            },
        }
    );
}

sub send_online {
    my ( $group ) = @_;

    return if !$char;

    send_spread(
        $group,
        $MSG_ONLINE_RES, {
            id     => unpack( 'V', $accountID ),
            online => $net->getState == Network::IN_GAME ? 1 : 0,
        }
    );
}

sub send_log_summary {
    my ( $group, $message_type, $log_type ) = @_;

    return if !$char;

    my $summary = {};
    foreach ( grep { $_->{type} eq $log_type } @$rolling_log ) {
        $summary->{ $_->{name} } += $_->{amount} || 0;
    }

    send_spread(
        $group,
        $message_type, {
            $log_type => $summary,
            id => unpack( 'V', $accountID ),
        }
    );
}

sub send_shop {
    my ( $group, $message_type ) = @_;

    return if !$char;
    return if !$shopstarted;

    my $shop = {
        id    => unpack( 'V', $accountID ),
        title => $shop{title},
        items => [ grep {$_} @articles ],
    };

    send_spread( $group, $message_type, $shop );
}

sub send_party_update {
    return if !$char;
    return if !$field;
    our $old;
    our $last_full_update ||= 0;
	my $statuses = 'none';
	if ($char->{statuses} && %{$char->{statuses}}) {
		$statuses = join ', ', keys %{$char->{statuses}};
	}
    my $pos = calcPosition($char);
    my $new = {
        ID         => unpack( 'V', $accountID ),
        party      => my_party(),
        partyadmin => $char->{party}->{users}->{$accountID}->{admin} ? 1                     : 0,
        hp         => $char->{hp},
        hp_max     => $char->{hp_max},
        map        => $field->baseName . '.gat',
        x          => $pos->{x},
        y          => $pos->{y},
        online     => $net->getState() == Network::IN_GAME           ? 1                     : 0,
        follow     => $config{follow}                                ? $config{followTarget} : '',
        sp         => $char->{sp},
        sp_max     => $char->{sp_max},
        lv         => $char->{lv},
        xp         => $char->{exp},
        xp_max     => $char->{exp_max},
        lv_job     => $char->{lv_job},
        jp         => $char->{exp_job},
        jp_max     => $char->{exp_job_max},
        zeny       => $char->{zeny},
        status     => $statuses,
        weight     => int $char->{weight},
        weight_max => $char->{weight_max},
        quest_counts => quest_counts(),
    };
    my $msg = {};
    my $time = Time::HiRes::time;
    if ($time - $last_full_update > 30) {
        $last_full_update = $time;
        $msg = $new;
    } else {
        # Send partial update.
        foreach (keys %$new) {
            next if $old->{$_} eq $new->{$_};
            $msg->{$_} = $new->{$_};
        }
        # Deeper diff for structures.
        foreach my $key ( qw( quest_counts ) ) {
            my $old = $old->{$key};
            my $new = $new->{$key};
            my $upd = $msg->{$key} = {};
            foreach ( keys %$new ) {
                next if $old->{$_} eq $new->{$_};
                $upd->{$_} = $new->{$_};
            }
            delete $msg->{$key} if !keys %$upd;
        }
        # Always send hp and hp_max together.
        if ( $msg->{hp} || $msg->{hp_max} ) {
            $msg->{hp}     = $new->{hp};
            $msg->{hp_max} = $new->{hp_max};
        }
        # Always send the account ID.
        $msg->{ID} = $new->{ID} if %$msg;
    }

    # No message if nothing changed.
    return if 1 == keys %$msg;

    send_spread( $bot_group, $MSG_PARTY_UPDATE, $msg ) if %$msg;
    $old = $new;
}

sub party_packet {
    return if !$char;
    my ($msg) = @_;

    # Fun stuff.
    message "party_packet $msg\n" if $debug > 1;
    my $args = $msg;
    return if !$args->{ID};

    my ($ID) = pack 'V', $args->{ID};
    if ($ID eq $accountID) {
        message "ignoring self message\n" if $debug > 1;
        return;
    }

    message "party_packet $args->{char}\n" if $debug;

    my $bot = $bot_party->{$ID} ||= {};
    foreach my $key ( keys %$msg ) {
        if ( ref $msg->{$key} eq 'HASH' ) {
            $bot->{$key} ||= {};
            $bot->{$key}->{$_} = $msg->{$key}->{$_} foreach keys %{ $msg->{$key} };
        } else {
            $bot->{$key} = $msg->{$key};
        }
    }

#message "[spread] seen $args->{char} [$args->{ID}] [$char->{party}{users}{$ID}] [$char->{party}{users}{$ID}{name}] [$char->{party}{users}{$ID}{seen}]\n" if $char->{party}{users}{$ID};
    # Add the player to our "party".
    my $actor = $char->{party}{users}{$ID};
    if ( $args->{char} ) {
        if ( binFind( \@partyUsersID, $ID ) eq '' ) {
            binAdd( \@partyUsersID, $ID );
        }
        if ( !$actor ) {
            $actor = $char->{party}{users}{$ID} = new Actor::Party;
            $actor->{ID} = $ID;

            $msg->{$_} ||= $bot->{$_} foreach keys %$bot;

            message "Party Member: $args->{char}\n";
        }
    }

    return if !$actor;
$actor->{seen} ||= 0;

    if ( exists $args->{party} ) {
        $actor->{bot} = $args->{party} ne my_party();
    }

    # Update name. This is needed on first join and on rejoin after dc.
    if ( $args->{char} ) {
        $actor->{name} = $args->{char};
    }

    # Update HP.
    if ( $args->{hp} && $args->{hp_max} ) {
        message "  hp $args->{hp}/$args->{hp_max}\n" if $debug;
        $packetParser->party_hp_info(
            {
                ID     => $ID,
                hp     => $args->{hp},
                hp_max => $args->{hp_max},
            }
        );
    }

    # Update map.
    if ( $args->{map} ) {
        message "  map $args->{map}\n" if $debug;
#message "[spread] setting map for $args->{char} to ($args->{map}) [$actor->{seen}]\n";
        $actor->{map} = $args->{map};
$actor->{seen} = 1;
    }

    # Update location.
    if ( $args->{x} || $args->{y} ) {
        message "  pos $args->{x},$args->{y}\n" if $debug;
#message "[spread] setting position for $args->{char} to ($args->{x},$args->{y}) [$actor->{seen}]\n";
        $actor->{pos}{x} = $args->{x} if $args->{x};
        $actor->{pos}{y} = $args->{y} if $args->{y};
$actor->{seen} = 2;
    }

    # Update online status.
    if ( exists $args->{online} ) {
        message "  online $args->{online}\n" if $debug;
        $actor->{online} = $args->{online} ? 1 : 0;
    }

    # Update "stuck" status.
    if ( exists $args->{stuck} ) {
        message "  stuck $args->{stuck}\n" if $debug;
        $actor->{stuck} = $args->{stuck} ? 1 : 0;
    }

    # Update follow target.
    if ( exists $args->{follow} ) {
        message "  follow $args->{follow}\n" if $debug;
        $actor->{follow} = $args->{follow};
        if ( $args->{follow} eq my_name() ) {
            $followers->{ $args->{char} } = 1;
        } else {
            delete $followers->{ $args->{char} };
        }
        $config{followers} = join ',', sort keys %$followers;
    }

    $actor->{last_update} = Time::HiRes::time;

    keep_group_together();

    return;
}

sub keep_group_together {
    my (undef, $args) = @_;

    return if $net->getState != Network::IN_GAME;

    our $last_group_update ||= 0;
    my $now = Time::HiRes::time;
    next if $last_group_update + 0.5 > $now;
    $last_group_update = $now;

    # Find members who are online and in the same follow-group.
    my @group;
    foreach my $c ( values %{ $char->{party}->{users} } ) {
        if ( $c->{follow} && $c->{follow} eq my_name() ) {
        } elsif ( $config{follow} && $config{followTarget} && $config{followTarget} eq $c->{name} ) {
        } elsif ( $config{follow} && $config{followTarget} && $c->{follow} && $config{followTarget} eq $c->{follow} ) {
        } else {
            next;
        }

        # Out of date information.
        next if $c->{last_update} < $now - 1.3;

        # They're on a different map. Let the follow code take care of it.
        next if !$c->{map} || $c->{map} ne $field->baseName . '.gat';

        # If we don't have position info, we can't calculate distance.
        next if !$c->{pos} || !$c->{pos}->{x} || !$c->{pos}->{y};

        # Not logged in.
        next if !$c->{online};

        # Pre-calc distance.
        my ( $cplayer ) = grep { $_->{ID} eq $c->{ID} } @{ $playersList->getItems };
        my $c2 = $cplayer || $c;
        my $cpos = $c2->{pos_to}->{x} ? calcPosition($c2) : $c2->{pos};
        $c->{dist} = Utils::distance( $cpos, calcPosition($char) );

        push @group, $c;
    }

    my $move_to;
    my $wait = 0;

    my @stuck = sort { $a->{dist} <=> $b->{dist} } grep { $_->{stuck} && $_->{dist} > 10 } @group;
    if (@stuck) {
        $move_to = $stuck[0];
    }

    # Leader should move towards followers which are very far away.
    my $move_dist   = $config{spread_followerMoveDist};
    my $move_amount = $config{spread_followerMoveAmount};
    my @far_away = sort { $b->{dist} <=> $a->{dist} } @group;
    if ( @far_away && $far_away[0]->{dist} >= $config{spread_followerMoveDist} ) {
        $move_to ||= $far_away[0];
    }

    # Leader should wait for slowpokes.
    my $wait_dist = $config{spread_followerWaitDist};
    my $wait_time = $config{spread_followerWaitTime};
    if ( !$config{follow} && @far_away && $far_away[0]->{dist} > $wait_dist ) {
        $wait = join ', ', map { $_->{name} } @far_away;
    }

    if ( !$config{follow} && $move_to && $AI == AI::AUTO ) {
        if ( !$far_follower || $far_follower->{name} ne $move_to->{name} ) {
            message "spread: Group member (" . $move_to->{name} . ") at $move_to->{pos}->{x} $move_to->{pos}->{y} (dist: $move_to->{dist}) is a long ways off.\n";
            $far_follower = {
                name    => $move_to->{name},
                state   => 'wait_to_start',
                timeout => { time => time, timeout => 4 },
            };
        }

        if ( $far_follower->{state} eq 'routing' && timeOut( $far_follower->{timeout} ) ) {
            $far_follower->{state} = 'wait_to_start';
        }

        if ( $far_follower->{state} eq 'wait_to_start' && timeOut( $far_follower->{timeout} ) ) {
            message "spread: Moving towards group member (" . $move_to->{name} . ") at $move_to->{pos}->{x} $move_to->{pos}->{y} (dist: $move_to->{dist})\n";

            # We can end up with a bunch of these routes and confuse Kore.
            # Clear any old ones.
            @ai_seq      = map { $ai_seq[$_] } grep      { !$ai_seq_args[$_]->{spreadFollow} } 0 .. $#ai_seq;
            @ai_seq_args = map { $ai_seq_args[$_] } grep { !$ai_seq_args[$_]->{spreadFollow} } 0 .. $#ai_seq_args;

            main::ai_route(
                $field->baseName, $move_to->{pos}->{x}, $move_to->{pos}->{y},
                attackOnRoute     => 0,
                noSitAuto         => 1,
                notifyUponArrival => 1,
                distFromGoal      => int $move_to->{dist} * ( 1 - $move_amount ),
            );
            AI->args->{spreadFollow} = 1;

            $far_follower->{state} = 'routing';
            $far_follower->{timeout} = { time => time, timeout => 5 };
        }
    } elsif ( $wait && AI::action eq 'route' ) {
        message "spread: Waiting ${wait_time}s for slowpokes to catch up: $wait\n";
        ai_clientSuspend( 0, $wait_time );
        # AI::clear(qw/move route/);
    } else {
        $far_follower = undef;
    }

}

sub join_group {
    my ($group) = @_;
    $group ||= $bot_group;
    Spread::join( mbox(), $group );
}

sub mbox {
    # Set timeout on connection failure, or kore will hang.
    our $spread_connect_timeout ||= { time => 0, timeout => 10 };

    our $spread_server ||= $config{spreadServer} ||= '4803@10.1.0.1';
    return if !$spread_server;
    return $spread_mbox if $spread_mbox && $spread_server eq $config{spreadServer};

    if ( $spread_mbox ) {
        message "spread: Disconnecting.\n";
        Spread::disconnect( $spread_mbox );
        $spread_mbox = undef;
    }
    $spread_server = $config{spreadServer};
    return if !$config{spreadServer};

    return if !timeOut( $spread_connect_timeout );

    Log::message( "spread: Connecting to spread server [$spread_server].\n" );
    ( $spread_mbox ) = Spread::connect( { spread_name => $spread_server, private_name => "pid$$" } );

    if ( !$spread_mbox ) {
        $spread_connect_timeout->{time} = time;
        Log::message( "spread: Connection failed [$!].\n" );
        return;
    }

    Log::message( "spread: Connected.\n" );
    join_group( $bot_group );
    join_group( $log_group );
    return $spread_mbox;
}

sub rolling_log {
    my ( $type, $name, $amount ) = @_;

    my $time = time;

    # Basically a priority queue of items based on time.
    push @$rolling_log, {
        time   => $time,
        type   => $type,
        name   => $name,
        amount => $amount || 1,
        };

    # Always prune old entries on insert.
    shift @$rolling_log while $rolling_log->[0]->{time} <= $time - 3600;
}

1;
