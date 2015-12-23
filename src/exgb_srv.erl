-module(exgb_srv).

-author('berner.setterwall@gmail.com').

-behaviour(gen_server).
%%-include("cecho.hrl").
-include("exgb_commands.hrl").

%% Behaviour Callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3]).

%% Module API
-export([start_link/0, call/2]).

%% Records
-record(state, { port, getch, observer }).

%% =============================================================================
%% Module API
%% =============================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_args, []).

call(Cmd, Args) ->
    gen_server:call(?MODULE, {call, Cmd, Args}, infinity).

%% =============================================================================
%% Behaviour Callbacks
%% =============================================================================
init(no_args) ->
    process_flag(trap_exit, true),
    case load_driver() of
	ok ->
	    Port = erlang:open_port({spawn, "exgb"}, [binary]),
	    {ok, #state{ port = Port }};
	{error, ErrorCode} ->
	    exit({driver_error, erl_ddll:format_error(ErrorCode)})
    end.

handle_call({call, Cmd, Args}, _From, State) ->
    {reply, do_call(State#state.port, Cmd, Args), State}.

terminate(_Reason, State) ->
    erlang:port_close(State#state.port),
    erl_ddll:unload("exgb").

handle_info({_Port, {data, _Binary}}, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
code_change(_, State, _) ->
    {noreply, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================
do_call(Port, Cmd) ->
    do_call(Port, Cmd, undefined).

do_call(Port, Cmd, Args) ->
    Port ! {self(), {command, term_to_binary({Cmd, Args})}},
    receive
        {Port, {data, Bin}} ->
            binary_to_term(Bin)
    end.

load_driver() ->
    Dir = case code:priv_dir(exgb) of
              {error, bad_name} ->
                  filename:dirname(code:which(?MODULE)) ++ "/../priv";
              D ->
                  D
          end,
    erl_ddll:load(Dir, "exgb").
