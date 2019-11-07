


game(Player1, Player2) :-
      tabuleiroInicial(Board), %fazer isto random?
      display_game(Board),
      mainLoop(Board),
      write('ok\n').
      %mainLoop(Board, Player1, Player2).

mainLoop(Board):-
    write('> White Player\'s turn...\n'),
    askCoordsWhite(Board, NewBoard),
    display_game(NewBoard),
    write('> Black Player\'s turn...\n'),
    askCoordsBlack(NewBoard, FinalBoard),
    display_game(FinalBoard),
    mainLoop(FinalBoard).






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






whiteTurn(Board, NewBoard, Row, Column):-
    getPeca(Row, Column, Board, Peca),
    checkWhiteCoord(Row, Column, Board, Peca),
    askBlocks(Number),
    makeMovement(Row, Column, Board, Number, Peca, NewBoard).

askCoordsWhite(Board, NewBoard):-
    askRow(NewRow),
    nl,
    askColumn(NewColumn),
    whiteTurn(Board, NewBoard, NewRow, NewColumn).

%%%%%%%%%%%%%%%%%%%%%%%
blackTurn(Board, NewBoard, Row, Column):-
    getPeca(Row, Column, Board, Peca),
    checkBlackCoord(Row, Column, Board, Peca),
    askBlocks(Number),
    makeMovement(Row, Column, Board, Number, Peca, NewBoard).

askCoordsBlack(Board, NewBoard):-
    askRow(NewRow),
    nl,
    askColumn(NewColumn),
    blackTurn(Board, NewBoard, NewRow, NewColumn).

%%%%%%%%%%%%%%%%%%%%%%%
 makeMovement(Row, Column, Board, Steps, Peca, NewBoard):-
    write('\nentrou\n'),
    (
        Column == 1 ->
        setPeca(Row, Column, null, Board, Board1),
        setPeca(Row, Column + Steps, Peca, Board1, NewBoard),
        true
        ;  write('')
    ),
    (
        Column == 8 ->
        setPeca(Row, Column, null, Board, Board1),
        setPeca(Row, Column - Steps, Peca, Board1, NewBoard), true
        ;  write('')
    ),
    (
        Row == 1 ->
        setPeca(Row, Column, null, Board, Board1),
        setPeca(Row + Steps, Column, Peca, Board1, NewBoard),true
        ;  write('')
    ),
    (
        Row == 8 ->
        setPeca(Row, Column, null, Board, Board1),
        setPeca(Row - Steps, Column, Peca, Board1, NewBoard),true
        ;  write('')
    ).


