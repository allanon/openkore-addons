sub alias_foo1 {
    'move 100 100',
    'move 110 110',
}

sub alias_foo2 {'
    move 100 100
    move 110 110
'}

sub alias_foo3 {
    cmd( 'move 100 100' );
    cmd( 'move 110 110' );
}

sub alias_foo4 {
    cmd( sub { 'move 100 100' } );
    cmd( 'move 110 110' );
}

sub alias_foo5 {
    sub { 'move 100 100' },
    'move 110 110',
}

sub alias_foo6 {
    foreach2 { 1..3 } loop {
      Log::message("foreach2 $_\n");
      "move 100 10$_"
    };
    cmd 'conf autoTour 0';
    if2 { $config{autoTour} } then {
      Log::message("if2\n");
      "move 105 105"
    };
    cmd 'conf autoTour 1';
    cmd 'move 110 110';
}
