menu :-
    displayMenu,
    write('> Choose an option'),
    nl,
    read(Input),
    manageInput(Input).


manageInput(0) :-
    nl,
    write('Leaving'),
    nl.

manageInput(1) :-
    write('Starting game ...\n'),
    game('P','P'),
    write('Thanks for playing!\n').

displayMenu :-
    nl,nl,
    write(' _______________________________________________________ '),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write('|                        F U S E                        |'),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write('|                        1 -> PvP                       |'),nl,
    write('|                                                       |'),nl,
    write('|                        0 -> Exit                      |'),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write(' _______________________________________________________ '),nl
    ,nl.