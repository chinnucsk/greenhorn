%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Main Greenhorn application.
%%% @end
%%%-----------------------------------------------------------------------------
-module(gh_app).

-include("gh.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================
start(_StartType, _StartArgs) ->
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

    gh_sup:start_link().

stop(_State) ->
    ok.
