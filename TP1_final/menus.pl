menu :-
    displayMenu,
    write('> Choose an option'),
    nl,
    read(Input),
    manageInput(Input).

goToMenu(Input):-
    write('\nPress [0] to go back to MAIN MENU.\n\n'),
    read(Input),
    travelBack(Input).

travelBack(0):-
    menu.

manageInput(0) :-
    write('\nExiting FUSE').

manageInput(1) :-
    write('\33\[2J'),
    write('Starting Player(W) vs Player(B) game\n'),
    game(white, black),
    write('\n| Thanks for playing FUSE |\n\n').

manageInput(2) :-
    write('\33\[2J'),
    write('Starting Player(W) vs CPU(B) game\n'),
    game2(white, black, 0),
    write('\n| Thanks for playing FUSE |\n\n').

manageInput(3) :-
    write('\33\[2J'),
    write('Starting Player(W) vs CPU(B) game\n'),
    game2(white, black, 1),
    write('\n| Thanks for playing FUSE |\n\n').

manageInput(4) :-
    write('\33\[2J'),
    write('Starting CPU(W) vs CPU(B) game\n'),
    game3(white, black),
    write('\n| Thanks for playing FUSE |\n\n').



displayMenu :-
    nl,nl,
    write('\33\[2J'), %clear screen and display menu
    write(' _______________________________________________________ '),nl,
    write('|                                                       |'),nl,
    write('|        ########  ##     ##   ######   ########        |'),nl,
    write('|        ##        ##     ##  ##    ##  ##              |'),nl,
    write('|        ##        ##     ##  ##        ##              |'),nl,
    write('|        ######    ##     ##   ######   ######          |'),nl,
    write('|        ##        ##     ##        ##  ##              |'),nl,
    write('|        ##        ##     ##  ##    ##  ##              |'),nl,
    write('|        ##         #######    ######   ########        |'),nl,
    write('|_______________________________________________________|'),nl,
    write('|                                                       |'),nl,
    write('|                     [1]  P  vs  P                     |'),nl,
    write('|                     [2]  P  vs CPU (Very Easy)        |'),nl,
    write('|                     [3]  P  vs CPU (Recommended)      |'),nl,
    write('|                     [4] CPU vs CPU                    |'),nl,
    write('|                     [0] Exit                          |'),nl,
    write('|                                                       |'),nl,
    write('|_______________________________________________________|'),nl,nl.


