:- module(dictionary,[phrase/5, op/1]).

% valid operators and filters
op(and).
op(or).

filter(media).
filter(links).
filter(images).
filter(native_video).
filter(retweets).
filter(verified).
filter(quote).

% true if first element is valid operator/filter
operator([X | L],L,E, C,C) :- op(X).
filter_option([X | L],L,E,C,C) :- filter(X).

% options is true if input represents filter keywords combined with operators
options(L0,L3,E,C0,C3) :-
  filter_option(L0,L1,E,C0,C1),
  operator(L1,L2,E,C1,C2),
  options(L2, L3, E, C2, C3).
% options can be a single filter keyword without an operator
options([L],L1,_,C0,C1) :- filter_option([L],L1,_,C0,C1).

% keyword is true if the input is not an operator, a filter keyword, or the word "filter"
keyword([X|L], L, E, C, C) :- 
  \+ operator([X|L], L, E, C, C),
  \+ filter_option([X|L], L, E, C, C),
  X \== 'filter',
  check_keyword(X, R).

% op_phrase is true if the input is either a group of keywords (ex: grumpy cat)
op_phrase(L0,L2,E,C0,C2) :-
  keyword(L0,L1,E,C0,C1),
  op_phrase(L1, L2, E, C1, C2).
  %   or it is a group of keywords combined with operators (ex: nasa and space)
op_phrase(L0,L3,E,C0,C3) :-
  keyword(L0,L1,E,C0,C1),
  operator(L1,L2,E,C1,C2),
  op_phrase(L2, L3, E, C2, C3).
op_phrase([L],L1,_,C0,C1) :- keyword([L],L1,_,C0,C1). 
% the part following "filter" must be a non-empty list (the phrase cannot end with "filter")
op_phrase([filter | L], L, _, C0, C1) :- length(L, Length), Length > 0.

% phrase is true if the input is either op_phrase (query keywords + filter) followed by filter keywords
phrase(L0, L2, E, C0, C2) :-
  op_phrase(L0, L1, E, C0, C1),
  options(L1, L2, E, C1, C2).
  % or it is only op_phrase (with no filter keywords)
phrase(L0, L1, E, C0, C1) :- op_phrase(L0, L1, E, C0, C1).
% or it is only a single query keyword
phrase([L0], L1, E, C0, C1) :- op_phrase([L0], L1, E, C0, C1).

% check_keyword is true if word is alphabetical
% check_keyword convert to string, then convert to array, check if each char is alphabetic
check_keyword(X, R) :- atom_string(X, S), atom_chars(S, T), is_alphanumeric(T, R).
is_alphanumeric([], []).
is_alphanumeric([H|T], X) :- char_type(H, alpha), is_alphanumeric(T, X).

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
