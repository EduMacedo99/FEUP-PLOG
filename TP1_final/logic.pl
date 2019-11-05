game(Player1, Player2) :-
      tabuleiroInicial(Board), %fazer isto random?
      display_game(Board),
      askCoords(NewRow, NewColumn),
      checkWhiteCoord(NewRow, NewColumn, Board),
      write('ok\n').
      %mainLoop(WorkersBoard, Player1, Player2).

%mainLoop(Board, Player1, Player2):-



askCoords(NewRow, NewColumn):-
    askRow(NewRow),
    nl,
    askColumn(NewColumn),
    nl.

getPeca(NLinha, NColuna, Board, Peca):-
    nth1(NLinha, Board, NColuna),
    nth1(NColuna, Board, Peca).




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