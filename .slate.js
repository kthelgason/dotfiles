
S.src('dotfiles/.reslate.js');

slate.alias('hyper', 'ctrl;alt;cmd');

slate.configAll({
    defaultToCurrentScreen: true,
    nudgePercentOf: 'screenSize',
    resizePercentOf: 'screenSize',
    undoOps: [
        'active-snapshot',
        'chain',
        'grid',
        'layout',
        'move',
        'resize',
        'sequence',
        'shell',
        'push'
    ]
});

// bindings

slate.bindAll({
hyper: {
    shift: {
        // edges
        h: [$('barResize', 'left',   3),
            $('center',    'left',   3, 3)],
        j: [$('barResize', 'bottom', 2),
            $('center',    'bottom', 3, 3)],
        k: [$('barResize', 'top',    2),
            $('center',    'top',    3, 3)],
        l: [$('barResize', 'right',  3),
            $('center',    'right',  3, 3)],

        // corners
        y: [$('corner', 'top-left',     3, 2),
            $('corner', 'top-left',     3, 3)],
        i: [$('corner', 'top-right',    3, 2),
            $('corner', 'top-right',    3, 3)],
        b: [$('corner', 'bottom-left',  3, 2),
            $('corner', 'bottom-left',  3, 3)],
        m: [$('corner', 'bottom-right', 3, 2),
            $('corner', 'bottom-right', 3, 3)],

            // centers
        u: [$('center', 'top'),
            $('center', 'top', 3, 3)],
        n: [$('center', 'bottom'),
            $('center', 'bottom', 3, 3)],
            'return': $('center', 'center', 3, 3)
    },
    // bars
    h: [$('barResize', 'left',  2),
        $('barResize', 'left',  1.5)],
    j: $('barResize', 'bottom', 2),
    k: $('barResize', 'top',    2),
    l: [$('barResize', 'right', 2),
        $('barResize', 'right', 1.5)],
        // corners
    y: [$('corner', 'top-left'),
        $('corner', 'top-left', 1.5)],
    i: [$('corner', 'top-right'),
        $('corner', 'top-right', 1.5)],
    b: [$('corner', 'bottom-left'),
        $('corner', 'bottom-left', 1.5)],
    m: [$('corner', 'bottom-right'),
        $('corner', 'bottom-right', 1.5)],
        // centers
        u: $('center', 'top'),
        n: $('center', 'bottom'),
        'return': $('center', 'center'),

        // direct focus 
        t: $.focus('Terminal'),
        f: $.focus('Finder'),
        // utility functions
        f1: 'relaunch',
        z: 'undo',
        tab: 'hint'
    }
});
