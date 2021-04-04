% Dictionary of our query

% operator(L, L, _, C, C).

% prep([has | L],L,_,C,C).
% prep([is | L],L,_,C,C).
% prep(L,L,_,C,C).

/*
happy or happiness retweet
grumpy cat or cat and dog
cat, retweet, and, something, or, cool
*/

/*
adjectives(L0,L2,Entity,C0,C2) :-
    adj(L0,L1,Entity,C0,C1),
    adjectives(L1,L2,Entity,C1,C2).
adjectives(L,L,_,C,C).
*/

/*
keyword has is op ... --> (grumpy and cat) has is
(happy birthday or grumpy cat and nasa) media retweet
*/
% (keyword operator keyword) filter:<special> filter:

phrase([], [], _, C,C).
% phrase(L0, L5, E1, C0, C5) :-
%   keyword(L0, L1, E1, C0, C1),
%   has_option(L1, L2, E1, C1, C2),
%   is_option(L2, L3, E1, C2, C3),
%   operator(L3, L4, E1, C3, C4),
%   phrase(L4, L5, E1, C4, C5).

% phrase(L0, L5, E1, C0, C5) :-
%   keyword(L0, L1, E1, C0, C1),
%   has_option(L1, L2, E1, C1, C2),
%   phrase(L2, L5, E1, C1, C5).
% phrase(L0, L5, E1, C0, C5) :-
%   keyword(L0, L1, E1, C0, C1),
%   is_option(L1, L2, E1, C1, C2),
%   phrase(L2, L5, E1, C1, C5).
% phrase(L0, L5, E1, C0, C4) :-
%   keyword(L0, L1, E1, C0, C1),
%   operator(L1, L2, E1, C1, C2),
%   keyword(L2, L3, E1, C2, C3),
%   phrase(L3, L5, E1, C3, C4).
% phrase(L0, L5, E1, C0, C4) :-
%   has_option(L0, L1, E1, C0, C1),
%   operator(L1, L2, E1, C1, C2),
%   keyword(L2, L3, E1, C2, C3),
%   phrase(L3, L5, E1, C3, C4).
% phrase(L0, L5, E1, C0, C4) :-
%   is_option(L0, L1, E1, C0, C1),
%   operator(L1, L2, E1, C1, C2),
%   keyword(L2, L3, E1, C2, C3),
%   phrase(L3, L5, E1, C3, C4).
% phrase(L0, L5, E1, C0, C5) :-
%   keyword(L0, L1, E1, C0, C1),
%   phrase(L1, L5, E1, C1, C5).

% phrase(L0, L5, E1, C0, C5) :-
%   has_option(L0, L1, E1, C0, C1),
%   phrase(L1, L5, E1, C1, C5).
% phrase(L0, L5, E1, C0, C5) :-
%   is_option(L0, L1, E1, C0, C1),
%   phrase(L1, L5, E1, C1, C5).
% phrase(L0, L5, E1, C0, C5) :-
%   operator(L0, L1, E1, C0, C1),
%   phrase(L1, L5, E1, C1, C5).
phrase([L0], L, E1, C0, C1) :- keyword([L0], L, E1, C0, C1).
phrase([L0], L, E1, C0, C1) :- has_option([L0], L, E1, C1, C2).
phrase([L0], L, E1, C0, C1) :- is_option([L0], L, E1, C2, C3).


% adj([large | L],L,Entity, [large(Entity)|C],C).
% has_option([media | L], L, Entity, [large(Entity)|C],C).
% has_option([links | L], ...)
% has_option([images | L], ...)





% operator([L], [], _, C, C) :- op(L).

% is_option([], [], _, C, C).
% is_option([X | L],L,Y,C,C) :- is(X).
% is_option([L], [], _, C, C) :- is(L).

% has_option([], [], _, C, C).
% has_option([X | L],L,Y,C,C) :- has(X).
% has_option([L], [], _, C, C) :- has(L).


% is_option([X | L],L,X, C,D) :- is(X).

% has_option([X | L], L) :- has(X).
% is_option([X | L], L) :- is(X).

% proper_noun([X | L],L,X, C,C) :- country(X).

% has_option([L0 | L1], L1, E1, [has(L0) | C1], C1) :- has(L0).

% keyword([], [], _, C, C).
keyword([L0 | L1], L1, E1, C0, C1) :-
  \+ operator([L0 | L1], L1, E1, C0, C1),
  \+ filter_option([L0 | L1], L1, E1, C0, C1).
keyword([L], L1, _, C, C) :-
  \+ operator([L], L1, E1, C0, C1),
  \+ filter_option([L], L1, E1, C0, C1).

operator([], [], _, C, C).
operator([X | L], L, _, C, C) :- op(X).

filter_option([], [], _, C, C).
filter_option([X | L],L,Y,C,C) :- filter(X).
filter_option([L], [], _, C, C) :- filter(L).

query(L0, L0, _, C, C).
query(L0, L3, E1, C0, C1) :-
  keyword(LO, L1, E1, C0, C1),
  operator(L1, L2, E1, C0, C1),
  query([H|L2], L3, E1, C0, C1).
query(L0, L2, E1, C0, C1) :-
  keyword(LO, L1, E1, C0, C1),
  query(L1, L2, E1, C0, C1).


query1([L0], L1, E1, C, C) :- keyword([L0], L1, E1, C, C).
query1(L0, L2, E1, C0, C1) :-
  keyword(LO, L1, E1, C0, C1),
  query1(L1, L2, E1, C0, C1).

query2([L0], [L0], _, C, C).
query2(L0, L3, E1, C0, C1) :-
  keyword(LO, L1, E1, C0, C1),
  operator(L1, L2, E1, C0, C1),
  query2([H|L2], L3, E1, C0, C1).

/*
adjectives(L0,L2,Entity,C0,C2) :-
    adj(L0,L1,Entity,C0,C1),
    adjectives(L1,L2,Entity,C1,C2).
adjectives(L,L,_,C,C).
*/

% has(media).
% has(links).
% has(images).

% is(reply).
% is(retweet).
% is(verified).
% is(quote).


filter(media).
filter(links).
filter(images).
filter(native_video).

filter(retweets).
filter(verified).
filter(quote).

op(and).
op(or).
op(not).

q(Ans) :-
    write("Keywords: "), flush_output(current_output),
    readln(Ln),
    ask(Ln,Ans).



% xyz: large and small --> keywords: large, small operator: and

% ask(Q, A) :-
