


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
    nl,
    askColumn(NewColumn),
    whiteTurn(Board, NewBoard, NewRow, NewColumn).


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
    nl,
    askColumn(NewColumn),
    blackTurn(Board, NewBoard, NewRow, NewColumn).

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
                
            ;   write('') %TO DO MOVE FORWARD
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
                
            ;   write('') %TO DO MOVE FORWARD
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
                
            ;   write('') %TO DO MOVE FORWARD
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
                
            ;   write('') %TO DO MOVE FORWARD
    ).
    
