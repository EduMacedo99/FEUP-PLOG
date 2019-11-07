game(Player1, Player2) :-
      tabuleiroInicial(Board), %fazer isto random?
      display_game(Board),
      askCoords(Board, NewBoard),!,
      write('saiu\n'),
      display_game(NewBoard),
      write('ok\n').
      %mainLoop(Board, Player1, Player2).

%mainLoop(Board, Player1, Player2):-






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




askCoords(Board, NewBoard):-
    askRow(NewRow),
    nl,
    askColumn(NewColumn),
    whiteTurn(Board, NewBoard, NewRow, NewColumn).


 makeMovement(Row, Column, Board, Steps, Peca, NewBoard):-
    write('\nentrou\n'),
    (
        Column == 1 ->
        setPeca(Row, Column, null, Board, Board1),
        setPeca(Row, Column + Steps, Peca, Board1, NewBoard),
        display_game(NewBoard)
        ; 
        write('NAO DEU SET!\n')
    ).




whiteTurn(Board, NewBoard, Row, Column):-
    getPeca(Row, Column, Board, Peca),
    checkWhiteCoord(Row, Column, Board, Peca),
    askBlocks(Number),!,
    makeMovement(Row, Column, Board, Number, Peca, NewBoard),!.









% askCoords(NewRow, NewColumn):-
%     askRow(NewRow),
%     nl,
%     askColumn(NewColumn),
%     nl.


%  makeMovement(Row, Column, Board, Steps, Peca, NewBoard):-
%     write('\nentrou\n'),
%     (
%         Column == 1 ->
%         setPeca(Row, Column, null, Board, Board1),
%         setPeca(Row, Column + Steps, Peca, Board1, NewBoard)
%         ; 
%         write('Valid answer!\n')
%     ).

% whiteTurn(Board, NewBoard):-
%     askCoords(NewRow, NewColumn),
%     checkWhiteCoord(NewRow, NewColumn, Board, Peca),
%     askBlocks(Number),
%     makeMovement(NewRow, NewColumn, Board, Number, Peca, NewBoard).
