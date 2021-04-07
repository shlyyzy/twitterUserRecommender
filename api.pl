:- module(api,[search/4]).

:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

% get_bearer_token is true if there is an env var set for the bearer token
get_bearer_token(T) :- getenv("TWITTER_BEARER_TOKEN", T).

/* 	
	sends a request to the Twitter API to get a list of recommended users based on the
	user provided query. search is True if the request is successful and
	if there are a list of recommended users given the query
*/
search(Search,RecommendedUsers,ErrorCode, NumResults):-
	get_bearer_token(BToken),
	Path='/1.1/search/tweets.json',
	SearchQuery=[q(Search), result_type='popular'],
	get_response(Path, SearchQuery, BToken, RecommendedUsers, ErrorCode, NumResults).

/* 	
	get_response is true if there is a response from the query from Twitter API
	and the response is successful. RecommendedUsers is the list of users
	based on the query returned by the search API
*/
get_response(Path, SearchQuery, BToken, RecommendedUsers, ErrorCode, NumResults) :-
	URL=[scheme(https), host('api.twitter.com'), path(Path), search(SearchQuery)],
	Options=[	authorization(bearer(BToken)),
				status_code(ErrorCode)
			],
	setup_call_cleanup(http_open(URL, In, Options),
					   json_read_dict(In, JSON),
					   close(In)), % cleans up response and gets JSON
	is_dict(JSON),
	ErrorCode is 200, % ensure response is valid
	get_screen_names(JSON.statuses, RecommendedUsers, NumResults).

/*
	get_screen_names is true if there is a list of screen names. gets the
	screen names from the status data from the search API
*/
get_screen_names([], [], 0).
get_screen_names([H|T], R, M) :- get_screen_names(T, R, N), member(H.user.screen_name, R), M is N+1.
get_screen_names([H|T], [H.user.screen_name | R], M) :- get_screen_names(T, R, N), \+ member(H.user.screen_name, R), M is N+1.

% examples of searches
% search('walrus', RecommendedUsers, ErrorCode, Num).
% search('nasa and space filter: media', RecommendedUsers, ErrorCode, Num).
% search('grumpy cat OR cat filter:images', RecommendedUsers, ErrorCode, Num).
