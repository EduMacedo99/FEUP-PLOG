game(Player1, Player2) :-
      tabuleiroFinal2(Board),
      display_game(Board),
      mainLoop(Board),
      write('ok\n').
      %mainLoop(Board, Player1, Player2).

checkGameOver(Board) :-
    checkGameOverTop(Board, 1, 1), %check row 1 between 2 and 7
    checkGameOverRight(Board, 1, 8),
    checkGameOverBottom(Board, 8, 1), 
    checkGameOverLeft(Board, 1, 1),
    write('=======================\n'),
    write('===    GAME OVER    ===\n'),
    write('=======================\n'). 

    % game over

mainLoop(Board):-
    checkGameOver(Board);
    (write('> White Player\'s turn...\n'),
        askCoordsWhite(Board, NewBoard),
        display_game(NewBoard),

    checkGameOver(Board);
    (write('> Black Player\'s turn...\n'),
        askCoordsBlack(NewBoard, FinalBoard),
        display_game(FinalBoard),

    mainLoop(FinalBoard))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkGameOverTop(_, _, 7).
checkGameOverTop(Board, RowIndex, ColIndex):-
    NextColIndex is ColIndex + 1,
    getPeca(RowIndex, NextColIndex, Board, EndPeca),
    EndPeca == empty,
    checkGameOverTop(Board, RowIndex, NextColIndex).
checkGameOverTop(Board, RowIndex, ColIndex):-
    NextColIndex is ColIndex + 1,
    getPeca(RowIndex, NextColIndex, Board, EndPeca),
    EndPeca \= empty,
    checkColFullDown(Board, 2, NextColIndex),
    checkGameOverTop(Board, RowIndex, NextColIndex).

checkGameOverRight(_, 7, _).
checkGameOverRight(Board, RowIndex, ColIndex):-
    NextRowIndex is RowIndex + 1,
    getPeca(NextRowIndex, ColIndex, Board, EndPeca),
    EndPeca \= empty,
    checkRowFullLeft(Board, NextRowIndex, 7),
    checkGameOverRight(Board, NextRowIndex, ColIndex).
checkGameOverRight(Board, RowIndex, ColIndex):-
    NextRowIndex is RowIndex + 1,
    getPeca(NextRowIndex, ColIndex, Board, EndPeca),
    EndPeca == empty,
    checkGameOverRight(Board, NextRowIndex, ColIndex).

checkGameOverBottom(_, _, 7).
checkGameOverBottom(Board, RowIndex, ColIndex):-
    NextColIndex is ColIndex + 1,
    getPeca(RowIndex, NextColIndex, Board, EndPeca),
    EndPeca == empty,
    checkGameOverBottom(Board, RowIndex, NextColIndex).
checkGameOverBottom(Board, RowIndex, ColIndex):-
    NextColIndex is ColIndex + 1,
    getPeca(RowIndex, NextColIndex, Board, EndPeca),
    EndPeca \= empty,
    checkColFullUp(Board, 7, NextColIndex),
    checkGameOverBottom(Board, RowIndex, NextColIndex).

checkGameOverLeft(_, 7, _).
checkGameOverLeft(Board, RowIndex, ColIndex):-
    NextRowIndex is RowIndex + 1,
    getPeca(NextRowIndex, ColIndex, Board, EndPeca),
    EndPeca \= empty,
    checkRowFullRight(Board, NextRowIndex, 2),
    checkGameOverLeft(Board, NextRowIndex, ColIndex).
checkGameOverLeft(Board, RowIndex, ColIndex):-
    NextRowIndex is RowIndex + 1,
    getPeca(NextRowIndex, ColIndex, Board, EndPeca),
    EndPeca == empty,
    checkGameOverLeft(Board, NextRowIndex, ColIndex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkColFullDown(_, 8, _).
checkColFullDown(Board, row, col):-
    getPeca(row, col, Board, endpeca),
    endpeca \= empty,
    nextrow is row + 1,
    checkColFull(Board, nextrow, col).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkColFullUp(_, 1, _).
checkColFullUp(Board, row, col):-
    getPeca(row, col, Board, endpeca),
    endpeca \= empty,
    nextrow is row - 1,
    checkColFull(Board, nextrow, col).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkRowFullRight(_, _, 8).
checkRowFullRight(Board, row, col):-
    getPeca(row, col, Board, endpeca),
    endpeca \= empty,
    nextcol is col + 1,
    checkRowFull(Board, row, nextcol).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
checkRowFullLeft(_, _, 1).
checkRowFullLeft(Board, row, col):-
    getPeca(row, col, Board, endpeca),
    endpeca \= empty,
    nextcol is col - 1,
    checkRowFull(Board, row, nextcol).

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
    (
        Column == 1 ->
        makeMovementRight(Row, Column, Board, Number, Peca, NewBoard)
        ; write('')
    ),
    (
        Column == 8 ->
        makeMovementLeft(Row, Column, Board, Number, Peca, NewBoard)
        ; write('')
    ),
    (
        Row == 1 ->
        makeMovementDown(Row, Column, Board, Number, Peca, NewBoard)
        ; write('')
    ),
    (
        Row == 8 ->
        makeMovementUp(Row, Column, Board, Number, Peca, NewBoard)
        ; write('')
    ).




askCoordsWhite(Board, NewBoard):-
    askRow(NewRow),
    % verificar se ha peca branca nesta row
    nl,
    askColumn(NewColumn),
    % verificar se ha peca branca nesta column
    whiteTurn(Board, NewBoard, NewRow, NewColumn).

askCoordsWhite(Board, NewBoard):-
    askCoordsWhite(Board, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%
blackTurn(Board, NewBoard, Row, Column):-
    getPeca(Row, Column, Board, Peca),
    checkBlackCoord(Row, Column, Board, Peca),
    askBlocks(Number),
    (
        Column == 1 ->
        makeMovementRight(Row, Column, Board, Number, Peca, NewBoard)
        ; write('')
    ),
    (
        Column == 8 ->
        makeMovementLeft(Row, Column, Board, Number, Peca, NewBoard)
        ; write('')
    ),
    (
        Row == 1 ->
        makeMovementDown(Row, Column, Board, Number, Peca, NewBoard)
        ; write('')
    ),
    (
        Row == 8 ->
        makeMovementUp(Row, Column, Board, Number, Peca, NewBoard)
        ; write('')
    ).

askCoordsBlack(Board, NewBoard):-
    askRow(NewRow),
    % verificar se ha peca black nesta row
    nl,
    askColumn(NewColumn),
    % verificar se ha peca black nesta column
    blackTurn(Board, NewBoard, NewRow, NewColumn).

askCoordsBlack(Board, NewBoard):-
    askCoordsBlack(Board, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%

decr(X, X1) :-
    X1 is X-1.

copy(L,R) :- accCp(L,R).
    accCp([],[]).

accCp([H|T1],[H|T2]) :- accCp(T1,T2).

move1stepRight(Row, Column, Board, Steps, Peca, NewBoard, NewStep):-
    setPeca(Row, Column, empty, Board, Board1),
    NewColumn is Column +1,
    setPeca(Row, NewColumn, Peca, Board1, NewBoard),
    decr(Steps, NewStep).

%%

move1stepLeft(Row, Column, Board, Steps, Peca, NewBoard, NewStep):-
    setPeca(Row, Column, empty, Board, Board1),
    NewColumn is Column - 1,
    setPeca(Row, NewColumn, Peca, Board1, NewBoard),
    decr(Steps, NewStep).

move1stepUp(Row, Column, Board, Steps, Peca, NewBoard, NewStep):-
    setPeca(Row, Column, empty, Board, Board1),
    NewRow is Row - 1,
    setPeca(NewRow, Column, Peca, Board1, NewBoard),
    decr(Steps, NewStep).

move1stepDown(Row, Column, Board, Steps, Peca, NewBoard, NewStep):-
    setPeca(Row, Column, empty, Board, Board1),
    NewRow is Row + 1,
    setPeca(NewRow, Column, Peca, Board1, NewBoard),
    decr(Steps, NewStep).
%%

 makeMovementRight(Row, Column, Board, Steps, Peca, NewBoard):-
    NextColumn is Column + 1,
    getPeca(Row, NextColumn, Board, Peca1),  
    (
            Peca1 == empty ->
                move1stepRight(Row, Column, Board, Steps, Peca, Board1, NewStep),
                (
                    NewStep == 0 ->
                        copy(Board1, NewBoard),
                        true
                        ;
                        NewColumn is Column + 1,
                        makeMovementRight(Row, NewColumn, Board1, NewStep, Peca, NewBoard)
                )
                
            ;   pushRight(Row, Column, Board, Peca1, TempBoard), %POSICAO DA PECAPRESA E Peca1 -> peca a frente
                makeMovementRight(Row, Column, TempBoard, Steps, Peca, NewBoard)
    ).

pushRight(Row, Column, Board, Peca, TempBoard):-
    NextColumn is Column +2,
    NewColumn is Column +1,
    getPeca(Row, NextColumn, Board, Peca2),
    (
        Peca2 == empty ->
        move1stepRight(Row, NewColumn, Board, 1, Peca, TempBoard, _NewStep)
        ; pushRight(Row, NewColumn, Board, Peca2, TempBoard)
    ).


    
 makeMovementLeft(Row, Column, Board, Steps, Peca, NewBoard):-
    NextColumn is Column - 1,
    getPeca(Row, NextColumn, Board, Peca1),  
    (
            Peca1 == empty ->
                move1stepLeft(Row, Column, Board, Steps, Peca, Board1, NewStep),
                (
                    NewStep == 0 ->
                        copy(Board1, NewBoard),
                        true
                        ;
                        NewColumn is Column - 1,
                        makeMovementLeft(Row, NewColumn, Board1, NewStep, Peca, NewBoard)
                )
                
            ;   pushLeft(Row, Column, Board, Peca1, TempBoard), %POSICAO DA PECAPRESA E Peca1 -> peca a frente
                makeMovementLeft(Row, Column, TempBoard, Steps, Peca, NewBoard)
    ).

pushLeft(Row, Column, Board, Peca, TempBoard):-
    NextColumn is Column -2,
    NewColumn is Column -1,
    getPeca(Row, NextColumn, Board, Peca2),
    (
        Peca2 == empty ->
        move1stepLeft(Row, NewColumn, Board, 1, Peca, TempBoard, _NewStep)
        ; pushLeft(Row, NewColumn, Board, Peca2, TempBoard)
    ).
    
 makeMovementUp(Row, Column, Board, Steps, Peca, NewBoard):-
    NextRow is Row - 1,
    getPeca(NextRow, Column, Board, Peca1),  
    (
            Peca1 == empty ->
                move1stepUp(Row, Column, Board, Steps, Peca, Board1, NewStep),
                (
                    NewStep == 0 ->
                        copy(Board1, NewBoard),
                        true
                        ;
                        NewRow is Row - 1,
                        makeMovementUp(NewRow, Column, Board1, NewStep, Peca, NewBoard)
                )
                
            ;   pushUp(Row, Column, Board, Peca1, TempBoard), %POSICAO DA PECAPRESA E Peca1 -> peca a frente
                makeMovementUp(Row, Column, TempBoard, Steps, Peca, NewBoard)
                
    ).

pushUp(Row, Column, Board, Peca, TempBoard):-
    NextRow is Row -2,
    NewRow is Row -1,
    getPeca(NextRow, Column, Board, Peca2),
    (
        Peca2 == empty ->
        move1stepUp(NewRow, Column, Board, 1, Peca, TempBoard, _NewStep)
        ; pushUp(NewRow, Column, Board, Peca2, TempBoard)
    ).
    
 makeMovementDown(Row, Column, Board, Steps, Peca, NewBoard):-
    NextRow is Row + 1,
    getPeca(NextRow, Column, Board, Peca1),  
    (
            Peca1 == empty ->
                move1stepDown(Row, Column, Board, Steps, Peca, Board1, NewStep),
                (
                    NewStep == 0 ->
                        copy(Board1, NewBoard),
                        true
                        ;
                        NewRow is Row + 1,
                        makeMovementDown(NewRow, Column, Board1, NewStep, Peca, NewBoard)
                )
                
            ;   pushDown(Row, Column, Board, Peca1, TempBoard), %POSICAO DA PECAPRESA E Peca1 -> peca a frente
                makeMovementDown(Row, Column, TempBoard, Steps, Peca, NewBoard)
    ).

pushDown(Row, Column, Board, Peca, TempBoard):-
    NextRow is Row +2,
    NewRow is Row +1,
    getPeca(NextRow, Column, Board, Peca2),
    (
        Peca2 == empty ->
        move1stepDown(NewRow, Column, Board, 1, Peca, TempBoard, _NewStep)
        ; pushDown(NewRow, Column, Board, Peca2, TempBoard)
    ).
    
