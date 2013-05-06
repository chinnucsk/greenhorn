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
-export([
         start/2,
         stop/1
        ]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================
start(_StartType, StartArgs) ->
    gh_sup:start_link().

stop(_State) ->
    ok.
