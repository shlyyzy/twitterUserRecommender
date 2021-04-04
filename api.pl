:- module(twitter,
         [token/1,
          search/4]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).

:- dynamic
	token/1.

% examples
% search('walrus', RecommendedUsers, ErrorCode, Num).
% search('(grumpy cat OR cat) OR (#meme has:images)', RecommendedUsers, ErrorCode, Num).
% "twitter data" has:mentions (has:media OR has:links)
% (nasa OR space) has:links
% twitter(has%3Amedia)

get_bearer_token(T) :- getenv("TWITTER_BEARER_TOKEN", T).

search(My_Search,RecommendedUsers,ErrorCode, M):-
	get_bearer_token(B_Token64),
	Path='/1.1/search/tweets.json',
	Search=[q(My_Search), result_type='popular'],
	% Search=[q(My_Search)],
	get_response(Path, Search, B_Token64, RecommendedUsers, ErrorCode, M).

get_response(Path, Search, B_Token64, RecommendedUsers, ErrorCode, M) :-
	URL=[scheme(https), host('api.twitter.com'), path(Path), search(Search)],
	Options=[	authorization(bearer(B_Token64)),
				status_code(ErrorCode)
			],
	setup_call_cleanup(http_open(URL, In, Options),
					   json_read_dict(In, JSON),
					   close(In)),
	is_dict(JSON),
	get_screen_names(JSON.statuses, RecommendedUsers, M).

get_screen_names([], [], 0).
get_screen_names([H|T], R, M) :- get_screen_names(T, R, N), member(H.user.screen_name, R), M is N+1.
get_screen_names([H|T], [H.user.screen_name | R], M) :- get_screen_names(T, R, N), \+ member(H.user.screen_name, R), M is N+1.

% get_screen_names([H|T], R, M) :- get_screen_names(T, R, N), member(H, R), M is N+1.
% get_screen_names([H|T], [H | R], M) :- get_screen_names(T, R, N), \+ member(H, R), M is N+1.