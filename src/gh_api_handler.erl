%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Greenhorn REST API handler
%%% @end
%%%-----------------------------------------------------------------------------
-module(gh_api_handler).

-include("gh.hrl").

%% API
-export([init/3,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         post_is_create/2,
         create_path/2,
         create_json/2,
         get_json/2,
         delete_resource/2
        ]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================
init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, create_json}
     ], Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, []}, get_json}
     ], Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(resource_id, Req) of
        {undefined, Req2} ->
            {true, Req2, all};
        {Id, Req2} ->
            {true, Req2, gh_utils:hex_to_bin(Id)}
    end.

post_is_create(Req, State) ->
    {true, Req, State}.

create_path(Req, State) ->
    {gh_utils:get_uuid(), Req, State}.

create_json(Req, Id) ->
    {true, Req, Id}.

get_json(Req, all) ->
    {Params, Req1} = cowboy_req:qs_vals(Req),
    {get_by_params(Params), Req1, all};
get_json(Req, Id) ->
    {get_by_id(Id), Req, Id}.

delete_resource(Req, Id) ->
    {true, Req, Id}.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
get_by_params(Params) ->
    [].

get_by_id(Id) ->
    [].
