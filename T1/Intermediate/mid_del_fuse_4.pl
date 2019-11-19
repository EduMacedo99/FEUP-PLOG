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
    
tabuleiroFinal([
    [null, empty, empty, empty, empty, white, empty, null],
    [empty, empty, empty, black, black, black, empty, empty],
    [empty, empty, white, empty, white, white, black, empty],
    [empty, black, black, black, black, black, white, empty],
    [empty, empty, empty, empty, white, black, white, empty],
    [empty, black, white, empty, white, white, white, empty],
    [empty, empty, empty, empty, black, white, empty, empty],
    [null, empty, empty, empty, empty, empty, empty, null]
    ]).


symbol(empty, '.').
symbol(null,' ').
symbol(black,'B').
symbol(white,'W').

display_game(Board, Player):-
    tabuleiroInicial(Board),
    format('It\'s ~p\'s turn ~n', [Player]),
    imprimeTabuleiro(Board).


imprimeTabuleiro([Head|Tail]) :-
    write('|'),
    imprimeLinha(Head),
    nl,
    imprimeTabuleiro(Tail).
imprimeTabuleiro([ ]).

imprimeLinha([Head|Tail]) :-
    symbol(Head,S),
    format(' ~s |', [S]),
    imprimeLinha(Tail).
imprimeLinha([ ]).


