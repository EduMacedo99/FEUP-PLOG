tabuleiroInicial([
    [null, black, white, black, black, white, white, null],
    [black, empty, empty, empty, empty, empty, empty, black],
    [white, empty, empty, empty, empty, empty, empty, white],
    [white, empty, empty, empty, empty, empty, empty, black],
    [black, empty, empty, empty, empty, empty, empty, white],
    [black, empty, empty, empty, empty, empty, empty, black],
    [white, empty, empty, empty, empty, empty, empty, white],
    [null, white, black, black, white, black, white, null]
    ]).
    
pos(0,'a').    
pos(1,'b').
pos(2,'c').
pos(3,'d').
pos(4,'e').
pos(5,'f').
pos(6,'g').
pos(7,'h').


symbol(empty, '.').
symbol(null,' ').
symbol(black,'B').
symbol(white,'W').

display_game(Board):-
    nl,
    write('   | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |\n'),
    write('---|---|---|---|---|---|---|---|---|\n'),
    imprimeTabuleiro(Board, 0).

imprimeTabuleiro([], 8).

imprimeTabuleiro([Head|Tail], Number) :-
    pos(Number, L),
    write(' '),
    write(L),
    Number1 is Number + 1,
    write(' |'),
    imprimeLinha(Head),
    write('\n---|---|---|---|---|---|---|---|---\n'),
    imprimeTabuleiro(Tail, Number1).

imprimeLinha([]).

imprimeLinha([Head|Tail]) :-
    symbol(Head,S),
    format(' ~s |', [S]),
    imprimeLinha(Tail).

