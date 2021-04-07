:- module(twitterUserRecommender).

:- use_module([dictionary(phrase)]).
:- use_module([dictionary(op)]).
% :- use_module(api).

q(Ans, Ln, Query, Filter) :-
    write("Query: "), flush_output(current_output),
    readln(Ln),
    get_constraints_from_question(Ln, A, C),
    get_parts(Ln, Q, F),
    create_filters(F, Filter),
    create_query(Q, Query).

get_constraints_from_question(Q,A,C) :-
    phrase(Q,End,A,C,[]), % A is the entity, C is the list of constraints we will build up
    member(End,[[]]). % end of question is either nothing or ? or . (lets you interpret ? as ending of a question)

get_parts(Ln, Q, F) :-
  member(filter, Ln),
  deconstruct(Ln, Q, F).
get_parts(Ln, Ln, []) :-
  \+ member(filter, Ln).

create_query([and|T], S) :- create_query(T, R), atom_concat(' AND', R, S).
create_query([or|T], S) :- create_query(T, R), atom_concat(' OR', R, S).
create_query([H|T], S) :-
    \+ op(H),
    create_query(T, R),
    atom_concat(' ', H, X),
    atom_concat(X, R, S). 
create_query([], "").

create_filters([H|T], S) :- \+ op(H), create_filters(T, R), atom_concat(' filter:', H, X), atom_concat(X, R, S). 
create_filters([and|T], S) :- create_filters(T, R), atom_concat(' AND', R, S).
create_filters([or|T], S) :- create_filters(T, R), atom_concat(' OR', R, S).
create_filters([], "").

deconstruct([H|T], [H | Q], X) :- deconstruct(T, Q, X).
deconstruct([filter | T], [], T).