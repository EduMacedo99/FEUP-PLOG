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

checkRow(_Row, NewRow):-
    write('Invalid Row!\n'),
    readRow(Input),
    checkRow(Input, NewRow).


askColumn(NewColumn) :-
    write('> Column?\n'),
    read(Column),
    checkRow(Column, NewColumn).

checkColumn('1', 1).

checkColumn('2', 2).
checkColumn('3', 3).
checkColumn('4', 4).
checkColumn('5', 5).
checkColumn('6', 6).
checkColumn('7', 7).
checkColumn('8', 8).

checkColumn(_Column, NewColumn):-
    write('Invalid Column!\n'),
    readColumn(Input),
    checkColumn(Input, NewColumn).

checkWhiteCoord(Row, Column, Board):-
    getPeca(Row, Column, Board, Peca),
    (   Peca \= white ->
    askCoords(Row, Column),
    write('not white!\n')
    ;   write('niceee\n')
    ).
