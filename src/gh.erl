%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Greenhorn entry point.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gh).

-include("gh.hrl").

%% API
-export([
         start_api/0,
         start_api/1,
         start_api/2,
         start_api/3,
         stop_api/0
        ]).

%%%=============================================================================
%%% API functions
%%%=============================================================================
start_api() ->
    Port = gh_utils:get_config_val(gh_port, 8080),
    NumAcceptors = gh_utils:get_config_val(gh_num_acceptors, 100),
    start_api(Port, NumAcceptors, []).

start_api(Routes) ->
    Port = gh_utils:get_config_val(gh_port, 8080),
    NumAcceptors = gh_utils:get_config_val(gh_num_acceptors, 100),
    start_api(Port, NumAcceptors, Routes).
start_api(Port, Routes) ->
    NumAcceptors = gh_utils:get_config_val(gh_num_acceptors, 100),
    start_api(Port, NumAcceptors, Routes).

start_api(Port, NumAcceptors, Routes) ->
    start_apps(),

    ?debug("gh: port=~p, num_acceptors=~p, routes=~p",
           [Port, NumAcceptors, Routes]),
    Port = gh_utils:get_config_val(gh_port, 8080),
    NumAcceptors = gh_utils:get_config_val(gh_num_acceptors, 100),

    Dispatch = [
                {'_',
                 [
                  {[<<"api">>], gh_api_handler, []}
                 ]
                }
               ],

    cowboy:start_http(
               http,
               NumAcceptors,
               [{port, Port}],
               [{dispatch, Dispatch}]
              ),
    ok.

stop_api() ->
    stop_apps().


%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
start_apps() ->
    [application:start(A) || A <- deps() ++ [gh]],
    ok.

stop_apps() ->
    [application:stop(A) || A <- lists:reverse(deps()) ++ [gh]],
    ok.

deps() ->
    [
     compiler, syntax_tools, lager, crypto, public_key, ssl,
     ranch, cowboy, jiffy, reloader
    ].
