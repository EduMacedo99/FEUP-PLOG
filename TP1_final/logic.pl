game(Player1, Player2) :-
      tabuleiroInicial(Board), %fazer isto random?
      display_game(Board),
      %dentro do white turn
      askCoords(NewRow, NewColumn),
      checkWhiteCoord(NewRow, NewColumn, Board, Peca),
      askBlocks(Number),
      makeMovement(NewRow, NewColumn, Peca, Number, Board, NewBoard),
    %   display_game(NewBoard),
      %fim de white turn
      write('ok\n').
      %mainLoop(WorkersBoard, Player1, Player2).

%mainLoop(Board, Player1, Player2):-

askCoords(NewRow, NewColumn):-
    askRow(NewRow),
    nl,
    askColumn(NewColumn),
    nl.

getPeca(NLinha, NColuna, Board, Peca):-
    nth1(NLinha, Board, Coluna),
    nth1(NColuna, Coluna, Peca).

setPeca(NLinha, NColuna, Peca, TabIn, TabOut):-
    setLinha(NLinha, NColuna, Peca, TabIn, TabOut).
    
setLinha(1, NColuna, Peca, [Linha | Resto], [NovaLinha | Resto]):-
    setColuna(NColuna, Peca, Linha, NovaLinha).

setLinha(N, NColuna, Peca, [Linha | Resto],[Linha | MaisLinhas]):-
    N > 1,
    Next is N-1,
    setLinha(Next, NColuna, Peca, Resto, MaisLinhas).

setColuna(1, Peca, [ _ | Resto],[Peca | Resto]).

setColuna(N, Peca, [X | Resto], [X | Mais]):-
    N > 1,
    Next is N - 1,
    setColuna(Next, Peca, Resto, Mais).

makeMovement(Row, Column, Peca, steps, Board, NewBoard):-
    write('entrou\n'),
    (Column == 1 ->
    write('column 1!\n')
    % setPeca(Row, Column + steps, Peca, Board, NewBoard)
    ;
    write('not col 1\n')
    ).
