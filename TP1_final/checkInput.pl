askRow(NewRow) :-
    write('> Row?\n'),
    read(Row),
    checkRow(Row, NewRow).


checkRow('a', 1).
checkRow('b', 2).
checkRow('c', 3).
checkRow('d', 4).
checkRow('e', 5).
checkRow('f', 6).
checkRow('g', 7).
checkRow('h', 8).

checkRow(_Column, _NewColumn):-
    write('> Invalid Row!\n'),
    fail.

askColumn(NewColumn) :-
    write('> Column?\n'),
    read(Column),
    checkColumn(Column, NewColumn).


checkColumn(1, 1).
checkColumn(2, 2).
checkColumn(3, 3).
checkColumn(4, 4).
checkColumn(5, 5).
checkColumn(6, 6).
checkColumn(7, 7).
checkColumn(8, 8).


checkColumn(_Column, _NewColumn):-
    write('Invalid Column!\n> Choose a new position!\n'),
    fail.

checkCoord(Player, Row, Column, Board, Peca):-
    getPeca(Row, Column, Board, Peca),
    (   Peca \= Player ->
    fail
    ;   write('')
    ).

% checkBlackCoord(Row, Column, Board, Peca):-
%     getPeca(Row, Column, Board, Peca),
%     (   Peca \= black ->
%     write('not black!\n'), 
%     fail
%     ;   write('Valid answer2!\n')
%     ).


askBlocks(Number):-
    write('\nHow many blocks do you wish to move?'),
    read(Number).