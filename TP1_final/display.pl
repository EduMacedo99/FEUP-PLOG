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
    
pos(0,' ').    
pos(1,'a').
pos(2,'b').
pos(3,'c').
pos(4,'d').
pos(5,'e').
pos(6,'f').
pos(7,' ').



symbol(empty, '.').
symbol(null,' ').
symbol(black,'B').
symbol(white,'W').

display_game(Board, Player):-
    tabuleiroInicial(Board),
    nl,
    write('       | 1 | 2 | 3 | 4 | 5 | 6 |   \n'),
    write('       |---|---|---|---|---|---|\n'),
    imprimeTabuleiro(Board, 0).


imprimeTabuleiro([Head|Tail], Number) :-
    pos(Number, L),
    write(' '),
    write(L),
    Number1 is Number + 1,
    write(' |'),
    imprimeLinha(Head),
    write('\n---|---|---|---|---|---|---|---|---\n'),
    imprimeTabuleiro(Tail, Number1).
imprimeTabuleiro([ ]).

imprimeLinha([Head|Tail]) :-
    symbol(Head,S),
    format(' ~s |', [S]),
    imprimeLinha(Tail).
imprimeLinha([ ]).


