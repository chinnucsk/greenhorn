%%%-----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Common utilities
%%% @end
%%%-----------------------------------------------------------------------------
-module(gh_utils).

-include("gh.hrl").

%% API
-compile(export_all).

%%%=============================================================================
%%% API functions
%%%=============================================================================
bin_to_hex(Bin) ->
    <<<<(binary:list_to_bin(
             case length(S = integer_to_list(I, 16)) of 1 -> [48|S]; 2 -> S end
          ))/bytes>> || <<I>> <= Bin>>.

hex_to_bin(Hex) when is_list(Hex) ->
    <<<<(list_to_integer([I1,I2], 16))>> || <<I1,I2>> <= list_to_binary(Hex)>>;
hex_to_bin(Hex) ->
    <<<<(list_to_integer([I1,I2], 16))>> || <<I1,I2>> <= Hex>>.

get_uuid() ->
    <<(crypto:rand_bytes(8))/bytes,
      (erlang:term_to_binary(erlang:now()))/bytes>>.

%% @doc Ensures that is binary
-spec to_bin(any()) -> binary().
to_bin(X) when is_list(X) -> list_to_binary(X);
to_bin(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_bin(X) when is_binary(X) -> X.

%% @doc Ensures that is atom
-spec to_atom(any()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X) when is_binary(X) -> list_to_atom(binary_to_list(X)).

%% @doc Ensures that is list
-spec to_list(any()) -> list().
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_float(X) -> float_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

%% @doc Ensures that this is integer
-spec to_int(any()) -> integer().
to_int(X) when is_binary(X) -> list_to_integer(binary_to_list(X));
to_int(X) when is_integer(X) -> X;
to_int(X) when is_float(X) -> list_to_integer(float_to_list(X));
to_int(X) when is_atom(X) -> list_to_integer(atom_to_list(X));
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(_) -> 0.

%% get parameter value from config
get_config_val(Param) ->
   get_config_val(Param, []).

get_config_val(Param, DefaultValue) ->
    case application:get_env(?APP, Param) of
        {_, ParamValue} ->
            ParamValue;
        _ ->
            DefaultValue
    end.

%%%=============================================================================
%%% Internal functionality
%%%=============================================================================
