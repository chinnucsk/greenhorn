%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Greenhorn entry point.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gh).

%% API
-export([start/0, stop/0]).

%%%=============================================================================
%%% API functions
%%%=============================================================================
-spec start() -> ok.
start() ->
    [application:start(A) || A <- deps() ++ [gh]],
    ok.

-spec stop() -> ok.
stop() ->
    [application:stop(A) || A <- lists:reverse(deps()) ++ [gh]],
    ok.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
deps() ->
    [
     compiler, syntax_tools, lager, crypto, public_key, ssl,
     ranch, cowboy, jiffy, reloader
    ].
