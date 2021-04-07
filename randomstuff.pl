random([H|T], [H | Q], X) :- random(T, Q, X).
random([filter | T], [], T).

create_filters([H|T], S) :- \+ op(H), create_filters(T, R), atom_concat(' filter:', H, X), atom_concat(X, R, S). 
create_filters([and|T], S) :- create_filters(T, R), atom_concat(' AND', R, S).
create_filters([or|T], S) :- create_filters(T, R), atom_concat(' OR', R, S).
create_filters([], "").

op(and).
op(or).
create_query([and|T], S) :- create_query(T, R), atom_concat(' AND', R, S).
create_query([or|T], S) :- create_query(T, R), atom_concat(' OR', R, S).
create_query([H|T], S) :-
    \+ op(H),
    create_query(T, R),
    atom_concat(' ', H, X),
    atom_concat(X, R, S). 
create_query([], "").
% ["a", "b"]
% random([X | T], T, []) :- notFilter(X).

% notFilter(X) :- X \== filter.
% append([],L,L).
% append([H|T],L,[H|R]) :-
%     append(T,L,R).