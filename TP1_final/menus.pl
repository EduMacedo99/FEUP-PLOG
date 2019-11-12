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

manageInput(2) :-
    menu.

displayMenu :-
    nl,nl,
    write(' _______________________________________________________ '),nl,
    write('|                                                       |'),nl,
    write('|                                                       |'),nl,
    write('|        ########  ##     ##   ######   ########        |'),nl,
    write('|        ##        ##     ##  ##    ##  ##              |'),nl,
    write('|        ##        ##     ##  ##        ##              |'),nl,
    write('|        ######    ##     ##   ######   ######          |'),nl,
    write('|        ##        ##     ##        ##  ##              |'),nl,
    write('|        ##        ##     ##  ##    ##  ##              |'),nl,
    write('|        ##         #######    ######   ########        |'),nl,
    write('|                                                       |'),nl,
    write('|                      1 -> P v P                       |'),nl,
    write('|                      2 -> P v CPU                     |'),nl,
    write('|                      3 -> P v CPU                     |'),nl,
    write('|                      0 -> Exit                        |'),nl,
    write('|                                                       |'),nl,
    write('|_______________________________________________________|'),nl
    ,nl.

%     /_/_/_/_/  /_/   /_/ / /_/_/_/    /_/_/_/_/
%    /_/        /_/   /_/  / /___      /_/       
%   /_/_/_/    /_/   /_/     /_ /     /_/_/_/    
%  /_/        /_/_ _/_/       /  /   /_/         
% /_/          /_/_/     _/_/_/_/   /_/_/_/_/