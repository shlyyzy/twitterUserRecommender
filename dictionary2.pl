op(and).
op(or).

filter(media).
filter(links).
filter(images).
filter(native_video).
filter(retweets).
filter(verified).
filter(quote).

operator([X | L],L,E, C,C) :- op(X).

filter_option([X | L],L,E,C,C) :- filter(X).

options(L0,L3,E,C0,C3) :-
  filter_option(L0,L1,E,C0,C1),
  operator(L1,L2,E,C1,C2),
  options(L2, L3, E, C2, C3).
options([L],L1,_,C0,C1) :- filter_option([L],L1,_,C0,C1).

keyword([X|L], L, E, C, C) :- 
  \+ operator([X|L], L, E, C, C),
  \+ filter_option([X|L], L, E, C, C),
  X \== 'filter'. 

op_phrase(L0,L2,E,C0,C2) :-
  keyword(L0,L1,E,C0,C1),
  op_phrase(L1, L2, E, C1, C2).
op_phrase(L0,L3,E,C0,C3) :-
  keyword(L0,L1,E,C0,C1),
  operator(L1,L2,E,C1,C2),
  op_phrase(L2, L3, E, C2, C3).
op_phrase([L],L1,_,C0,C1) :- keyword([L],L1,_,C0,C1).
op_phrase([filter | L], L, _, C0, C1).

% adj(['afghan'| T], T, 'categories=afghani').
phrase(L0, L2, E, C0, C2) :-
  op_phrase(L0, L1, E, C0, C1),
  options(L1, L2, E, C1, C2).
phrase(L0, L1, E, C0, C1) :- op_phrase(L0, L1, E, C0, C1).
phrase([L0], L1, E, C0, C1) :- op_phrase([L0], L1, E, C0, C1).

% grumpy cat AND space filter:media
q(Ans, Ln, Query, Filter) :-
    write("Query: "), flush_output(current_output),
    readln(Ln),
    get_constraints_from_question(Ln, A, C),
    get_parts(Ln, Q, F),
    create_filters(F, Filter),
    create_query(Q, Query).

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

% ask(Q,A) gives answer A to question Q
% ask(Q,A) :-
%     get_constraints_from_question(Q,A,C).

% get_constraints_from_question(Q,A,C) is true if C is the constaints on A to infer question Q
get_constraints_from_question(Q,A,C) :-
    phrase(Q,End,A,C,[]), % A is the entity, C is the list of constraints we will build up
    member(End,[[]]). % end of question is either nothing or ? or . (lets you interpret ? as ending of a question)

% prove_all(L) is true if all elements of L can be proved from the knowledge base
% prove_all([]).
% prove_all([H|T]) :-
%     call(H),      % built-in Prolog predicate calls an atom --> call(a constraint) tries to prove constraint using the KB, so call(country(argentina)). is T, call(country(turkey)). is F because it is not in the KB
%     prove_all(T).

/*
examples:
op_phrase([nasa, and, space], L1, E1, C0, C1).
op_phrase([nasa, and, space, and], L1, E1, C0, C1). //false
op_phrase([nasa, and], L1, E1, C0, C1). //false
op_phrase([nasa], L1, E1, C0, C1).
op_phrase([nasa, space], L1, E1, C0, C1). //false (need to put operator)

phrase([grumpy, cat, and, space, filter, media, and, retweets], L2, E, C0, C2).
phrase([grumpy, cat, and, space, filter], L2, E, C0, C2). --> no filter
phrase([grumpy, cat, and, space], L2, E, C0, C2). --> also no filter
*/

