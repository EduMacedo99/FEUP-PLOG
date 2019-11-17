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
    %return to menu function
    menu.

manageInput(0) :-
    %exit program upon input 0 in main menu.
    write('\n> Exiting FUSE').

manageInput(1) :-
    %Player vs Player upon input 1 in main menu (easy)
    write('\33\[2J'),
    write('Starting Player(W) vs Player(B) game\n'),
    game(white, black),
    write('\n| Thanks for playing FUSE |\n\n').

manageInput(2) :-
    %Player vs CPU upon input 2 in main menu (easy)
    write('\33\[2J'),
    write('Starting Player(W) vs CPU(B) game\n'),
    game2(white, black, 0),
    write('\n| Thanks for playing FUSE |\n\n').

manageInput(3) :-
    %Player vs CPU upon input 3 in main menu (normal)
    write('\33\[2J'),
    write('Starting Player(W) vs CPU(B) game\n'),
    game2(white, black, 1),
    write('\n| Thanks for playing FUSE |\n\n').

manageInput(4) :-
    %CPU vs CPU upon input 4 in main menu
    write('\33\[2J'), 
    write('Starting CPU(W) vs CPU(B) game\n'),
    game4(white, black),
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
    write('|                  [1]  P  vs  P                        |'),nl,
    write('|                  [2]  P  vs CPU (Very Easy)           |'),nl,
    write('|                  [3]  P  vs CPU (Recommended)         |'),nl,
    write('|                  [4] CPU vs CPU                       |'),nl,
    write('|                  [0] Exit                             |'),nl,
    write('|                                                       |'),nl,
    write('|_______________________________________________________|'),nl,nl.
