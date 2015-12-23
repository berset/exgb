-module(exgb).

-author('berner.setterwall@gmail.com').

-behaviour(application).
-include("include/exgb_commands.hrl").

%% Behaviour Callbacks
-export([start/2, stop/1]).

%% Application API

-export([ predict/2
        , load_model/1
        ]).

%% =============================================================================
%% Application API
%% =============================================================================
predict(Model, Features) when is_integer(Model) andalso is_list(Features) ->
    case lists:all(fun is_float/1, Features) of
        true ->
            call(?PREDICT, {Model, Features});
        false ->
            badarg
    end.

load_model(ModelPath) when is_list(ModelPath) ->
    call(?LOAD_MODEL, {string:len(ModelPath), ModelPath}).

%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
start(_, _) ->
    exgb_srv:start_link().

stop(_) ->
    ok.

%% =============================================================================
%% Internal Functions
%% =============================================================================
call(Cmd) ->
    call(Cmd, undefined).

call(Cmd, Args) ->
    exgb_srv:call(Cmd, Args).
