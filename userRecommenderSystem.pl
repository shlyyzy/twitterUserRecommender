:- module(userRecommenderSystem).
:- use_module(dictionary).
:- use_module(api).

/* 
  SET UP:
    export TWITTER_BEARER_TOKEN=<secret_bearer_token>
    TO SEE FULL LIST OF REC USERS: set_prolog_flag(answer_write_options,[max_depth(0)]).
    TO RUN: q(Ln, RecommendedUsers, NumResults)
    QUERY MUST BE STRUCTURED AS SO:
      [\w [or|and]+\w]* [[filter]+ [[media|links|images|native_video|retweets|verified|quote]+ [and|or]+]*]
*/

/* 
  q is true if query is valid and there is a successful req sent to the twitter API for a list of recommended users
  q accepts and validate an input query to filter Tweets, send it to the Twitter API, return back the recommended users
*/
q(Ln, RecommendedUsers, NumResults) :-
    write("Query: "), flush_output(current_output),
    readln(Ln),
    get_constraints_from_question(Ln, A, C),
    get_parts(Ln, Q, F),
    create_filters(F, Filter),
    create_query(Q, Query),
    create_search_query(Query, Filter, SearchQuery),
    search(SearchQuery, RecommendedUsers, ErrorCode, NumResults).

/* 
  create_search_query is true if Query and Filter can be concatenated
  create_search_query creates a valid search query to send to Twitter API by concatenating keywords with filter options
*/
create_search_query(Query, Filter, SearchQuery) :-
  atom_concat(Query, ' ', S),
  atom_concat(S, Filter, SearchQuery).

% get_constraints_from_question is true if it validates that the input query is a phrase
get_constraints_from_question(Q,A,C) :-
    phrase(Q,End,A,C,[]),
    member(End,[[]]).

/* 
  get_parts is true if it can split the input into query keywords and filter options
  if there's "filter", read everything before "filter" as the query Q, and everything after "filter" as the filter F
  if there's no "filter", read the whole input as the query Q
*/
get_parts(Ln, Q, F) :-
  member(filter, Ln),
  deconstruct(Ln, Q, F).
get_parts(Ln, Ln, []) :-
  \+ member(filter, Ln).

/*
  create_query is true if it can create a valid query to send to the Twitter API
  replace and with AND, or with OR to conform to the Twitter API
*/
create_query([and|T], S) :- create_query(T, R), atom_concat(' AND', R, S).
create_query([or|T], S) :- create_query(T, R), atom_concat(' OR', R, S).
% if not an operator, add spaces between the keywords
create_query([H|T], S) :-
    \+ op(H),
    create_query(T, R),
    atom_concat(' ', H, X),
    atom_concat(X, R, S). 
create_query([], "").

/* 
  create_filters is true if it can create a valid set of filter options to send to the Twitter API
  if not an operator, prepend filter: to the filter keyword
*/
create_filters([H|T], S) :- \+ op(H), create_filters(T, R), atom_concat(' filter:', H, X), atom_concat(X, R, S). 
% replace and with AND, or with OR to conform to the Twitter API
create_filters([and|T], S) :- create_filters(T, R), atom_concat(' AND', R, S).
create_filters([or|T], S) :- create_filters(T, R), atom_concat(' OR', R, S).
create_filters([], "").

% deconstruct returns everything before "filter" in the 2nd argument, and everything after "filter" in the 3rd argument
deconstruct([H|T], [H | Q], X) :- deconstruct(T, Q, X).
deconstruct([filter | T], [], T).

% sample query:
% grumpy cat and meme filter media
% nasa and space filter media or verified
% filter media
