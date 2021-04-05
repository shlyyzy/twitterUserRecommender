op(and).
op(or).
op(not).

filter(media).
filter(links).
filter(images).
filter(native_video).
filter(retweets).
filter(verified).
filter(quote).

operator([X | L],L,E, C,C) :- op(X).

filter_option([X | L],L,E,C,C) :- filter(X).

keyword([X|L], L, E, C, C) :- 
  \+ operator([X|L], L, E, C, C),
  \+ filter_option([X|L], L, E, C, C).

op_phrase(L0,L3,E,C0,C3) :-
  keyword(L0,L1,E,C0,C1),
  operator(L1,L2,E,C1,C2),
  op_phrase(L2, L3, E, C2, C3).
op_phrase([L],L1,_,C0,C1) :- keyword([L],L1,_,C0,C1).

/*
examples:
op_phrase([nasa, and, space], L1, E1, C0, C1).
op_phrase([nasa, and, space, and], L1, E1, C0, C1). //false
op_phrase([nasa, and], L1, E1, C0, C1). //false
op_phrase([nasa], L1, E1, C0, C1).
op_phrase([nasa, space], L1, E1, C0, C1). //false (need to put operator)
*/