game(Player1, Player2) :-
      tabuleiroInicial(Board), 
      display_game(Board),
      mainLoop(Board),
      write('ok\n').


game2(Player1, CPU) :-
      tabuleiroInicial(Board), 
      display_game(Board),
      mainLoop2(Board),
      write('ok\n').


mainLoop(Board):-
    write('> White Player\'s turn...\n'),
        askCoordsWhite(Board, NewBoard),
        display_game(NewBoard),

    write('> Black Player\'s turn...\n'),
        askCoordsBlack(NewBoard, FinalBoard),
        display_game(FinalBoard),

    mainLoop(FinalBoard).



mainLoop2(Board):-
    write('> White Player\'s turn...\n'),
        askCoordsWhite(Board, NewBoard),
        display_game(NewBoard),

    write('> Black Player\'s turn...\n'),
        findall([R,C,N], findall_aux(Board, R, C, N, black), ListOfOutputs),
        write('\n\n'),
        write(ListOfOutputs),
        write('\n\n'),
        nth1(1, ListOfOutputs, Elem),
        generateMove(Elem, NewBoard, FinalBoard),
        display_game(FinalBoard),

     mainLoop2(FinalBoard).
    

checkGameOverTop(Board, RowIndex, ColIndex):-
    NextColIndex is ColIndex + 1,
    (
        NextColIndex == 8 ->
        checkGameOverRight(Board, 1, 1)
        ; write('')
    ),
    getPeca(RowIndex, NextColIndex, Board, EndPeca),
    (
        EndPeca == empty -> 
        checkGameOverTop(Board, RowIndex, NextColIndex)
        ; checkColFull(Board, 2, NextColIndex)
    ).

checkGameOverRight(Board, RowIndex, ColIndex):-
    NextRowIndex is RowIndex + 1,
    (
        NextRowIndex == 8 ->
        checkGameOverBottom(Board, 1, 1)
        ; write('')
    ),
    getPeca(NextRowIndex, ColIndex, Board, EndPeca),
    (
        EndPeca == empty -> 
        checkGameOverRight(Board, NextRowIndex, ColIndex)
        ; 
        checkRowFull(Board, NextRowIndex, 2)
    ).

checkGameOverBottom(Board, RowIndex, ColIndex):-
    NextColIndex is ColIndex + 1,
    (
        NextColIndex == 8 ->
        checkGameOverLeft(Board, 1, 1)
        ; write('')
    ),
    getPeca(RowIndex, NextColIndex, Board, EndPeca),
    (
        EndPeca == empty -> 
        checkGameOverBottom(Board, RowIndex, NextColIndex)
        ; checkRowFull(Board, 2, NextColIndex)
    ).

checkGameOverLeft(Board, RowIndex, ColIndex):-
    NextRowIndex is RowIndex + 1,
    (
        NextRowIndex == 8 ->
        nl,
        write('=======================\n'),
        write('===    GAME OVER    ===\n'),
        write('=======================\n')
        ; write('')
    ),
    getPeca(NextRowIndex, ColIndex, Board, EndPeca),
    (
        EndPeca == empty -> 
        checkGameOverLeft(Board, NextRowIndex, ColIndex)
        ; checkColFull(Board, NextRowIndex, 2)
    ).


checkColFull(Board, row, col):-
    (
        (
            row == 8 ->
            nl,
            write('=======================\n'),
            write('===    GAME OVER    ===\n'),
            write('=======================\n')
            ; write('')
        ),

        getPeca(row, col, Board, endpeca),
        
        (
            endpeca \= empty ->
            nextrow is row + 1,
            checkColFull(Board, nextrow, col)
            ; write('hey')
        )
    ).

checkRowFull(Board, row, col):-
    (
        (
            nextcol == 8 ->
            nl,
            write('=======================\n'),
            write('===    GAME OVER    ===\n'),
            write('=======================\n')
            ; write('')
        ),
        getPeca(row, col, Board, endpeca),
        (
            endpeca \= empty -> 
            nextcol is col + 1,
            checkRowFull(Board, row, nextcol)
            ; write('hey')
        )        
    ).




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


checkValidPlay(Player, Board, Row, Column, Number):-
    getPeca(Row, Column, Board, Peca),
    checkCoord(Player, Row, Column, Board, Peca),
    (
        (
            Column == 1,
            checkValidStepRight(Row, 1, Board, Number, 0)
        );
        (
            Column == 8,
            checkValidStepLeft(Row, 8, Board, Number, 0)
        );
        (
            Row == 1,
            checkValidStepDown(1, Column, Board, Number, 0)
        );
        (
            Row == 8,
            checkValidStepUp(8, Column, Board, Number, 0)
        )
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FIND ALL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


findall_aux(Board, RowOut, ColOut, Number, Player) :-
    findall_aux_right(Board, 2, 8, 1, Player, RowOut, ColOut, Number).

findall_aux_right(Board, R, C, N, Player, RowOut, ColOut, NumOut) :-
     !,
    R < 8,
    (
        (
            checkValidPlay(Player, Board, R, C, N),
            RowOut is R,
            ColOut is C,
            NumOut is N
        );
        (
            (
                N > 5,
                NewR is R + 1, !, 
                findall_aux_right(Board, NewR, C, 1, Player, RowOut, ColOut, NumOut)
            );
            (
                NewN is N + 1, !, 
                findall_aux_right(Board, R, C, NewN, Player, RowOut, ColOut, NumOut)
            )
        )
    ).

findall_aux_left(Board, R, C, N, Player, RowOut, ColOut, NumOut) :-
    !,
    R < 8,
    (
        (
            checkValidPlay(Player, Board, R, C, N),
            RowOut is R,
            ColOut is C,
            NumOut is N
        );
        (
            (
                N > 5,
                NewR is R + 1, !, 
                findall_aux_left(Board, NewR, C, 1, Player, RowOut, ColOut, NumOut)
            );
            (
                NewN is N + 1, !, 
                findall_aux_left(Board, R, C, NewN, Player, RowOut, ColOut, NumOut)
            )
        )
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FIND ALL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


whiteTurn(Board, NewBoard, Row, Column, Number):-
    checkValidPlay(white, Board, Row, Column, Number),
    getPeca(Row, Column, Board, Peca),
    (
        (
            Column == 1,
            makeMovementRight(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Column == 8,
            makeMovementLeft(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Row == 1,
            makeMovementDown(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Row == 8,
            makeMovementUp(Row, Column, Board, Number, Peca, NewBoard)
        )
    ).




askCoordsWhite(Board, NewBoard):-
    askRow(NewRow),
    nl,
    askColumn(NewColumn),
    askBlocks(Number),
    whiteTurn(Board, NewBoard, NewRow, NewColumn, Number).

askCoordsWhite(Board, NewBoard):-
    askCoordsWhite(Board, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%
blackTurn(Board, NewBoard, Row, Column, Number):-
    checkValidPlay(black, Board, Row, Column, Number),
    getPeca(Row, Column, Board, Peca),
    (
        (
            Column == 1,
            makeMovementRight(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Column == 8,
            makeMovementLeft(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Row == 1,
            makeMovementDown(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Row == 8,
            makeMovementUp(Row, Column, Board, Number, Peca, NewBoard)
        )
    ).

askCoordsBlack(Board, NewBoard):-
    askRow(NewRow),
    nl,
    askColumn(NewColumn),
    askBlocks(Number),
    blackTurn(Board, NewBoard, NewRow, NewColumn, Number).

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

checkValidStepRight(Row, Column, Board, Steps, Counter) :-
    Column < 7,
    NextColumn is Column + 1,
    getPeca(Row, NextColumn, Board, Peca),
    (
        Peca \= empty,
        NewCounter is Counter + 1;
        NewCounter is Counter
    ),
    checkValidStepRight(Row, NextColumn, Board, Steps, NewCounter).

checkValidStepRight(Row, Column, Board, Steps, Counter) :-
    \+ (Steps > 6 - Counter).

checkValidStepLeft(Row, Column, Board, Steps, Counter) :-
    Column > 1,
    NextColumn is Column - 1,
    getPeca(Row, NextColumn, Board, Peca),
    (
        Peca \= empty,
        NewCounter is Counter + 1;
        NewCounter is Counter
    ),
    checkValidStepLeft(Row, NextColumn, Board, Steps, NewCounter).

checkValidStepLeft(Row, Column, Board, Steps, Counter) :-
    \+ (Steps > 6 - Counter).

checkValidStepDown(Row, Column, Board, Steps, Counter) :-
    Row > 1,
    NextRow is Row - 1,
    getPeca(NextRow, Column, Board, Peca),
    (
        Peca \= empty,
        NewCounter is Counter + 1;
        NewCounter is Counter
    ),
    checkValidStepDown(NextRow, Column, Board, Steps, NewCounter).

checkValidStepDown(Row, Column, Board, Steps, Counter) :-
    \+ (Steps > 6 - Counter).

checkValidStepUp(Row, Column, Board, Steps, Counter) :-
    Row < 7,
    NextRow is Row + 1,
    getPeca(NextRow, Column, Board, Peca),
    (
        Peca \= empty,
        NewCounter is Counter + 1;
        NewCounter is Counter
    ),
    checkValidStepUp(NextRow, Column, Board, Steps, NewCounter).

checkValidStepUp(Row, Column, Board, Steps, Counter) :-
    \+ (Steps > 6 - Counter).

generateMove(Elem, Board, NewBoard):-
    nth1(1, Elem, Row),
    nth1(2, Elem, Column),
    nth1(3, Elem, Number),
    getPeca(Row, Column, Board, Peca),
    (
        (
            Column == 1,
            makeMovementRight(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Column == 8,
            makeMovementLeft(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Row == 1,
            makeMovementDown(Row, Column, Board, Number, Peca, NewBoard)
        );
        (
            Row == 8,
            makeMovementUp(Row, Column, Board, Number, Peca, NewBoard)
        )
    ).



