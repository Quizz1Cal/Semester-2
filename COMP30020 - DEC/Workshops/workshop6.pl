% Workshop 6 - Uh oh Prolog is here

% To set-up prolog use this.
% working_directory(_, 'C:/Users/USER/Documents/GitHub/Semester-2/COMP30020 - DEC/Workshops').
% ensure_loaded('workshop6.pl')
% TODO: Look at syntactic sugar [].
% TO RELOAD FILES: make.
% TODO: Find a pseudo-predicate to pull up stuff.

% Q1 imports
:- ensure_loaded(borders).
:- ensure_loaded(cities).
:- ensure_loaded(countries).
:- ensure_loaded(rivers).

win(r,s).
win(s,p).
win(p,r).

% What borders Australia. TYPE INTO
:- borders(australia,X), borders(X, australia).

% What shares a border with France and Spain
:- borders(france,X), borders(spain, X).

% Q4
country(C) :- country(C,_,_,_,_,_,_,_).

% Q5
larger(Country1, Country2) :- 
    country(Country1,_,_,_,Area1,_,_,_), 
    country(Country2,_,_,_,Area2,_,_,_),
    Area1 >= Area2.

% Q6.
river_country(River, Country) :-
    river(River, Countries),
    country(Country),
    member(Country, Countries).


