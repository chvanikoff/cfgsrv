-module(cfgsrv).
-author('chvanikoff <chvanikoff@gmail.com>').

-behaviour(gen_server).

%% Gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%% API
-export([
	start_link/0,
	start_link/1,
	get/1, get/2,
	get_multiple/1,
	update/0, update/1
]).

-define(SERVER, ?MODULE).
-include("cfgsrv.hrl").

-record(state, {config}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() ->
	{ok, Pid :: pid()}.
start_link() ->
	start_link(?DEFAULT_CONFIG).

-spec start_link(Config :: string()) ->
	{ok, Pid :: pid()}.
start_link(Config) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

-spec get(Key :: atom() | string()) ->
	{ok, Result :: any()}.
get(Key) ->
	get(Key, ?DEFAULT_VALUE).

-spec get(Key :: atom() | string(), Default :: any()) ->
	{ok, Result :: any()}.
get(Key, Default) when is_atom(Key) ->
	get(atom_to_list(Key), Default);
get(Key, Default) ->
	gen_server:call(?SERVER, {get, Key, Default}).

-spec get_multiple(Keys :: [atom() | string() | {atom() | string(), any()}]) ->
	{ok, [Results :: any()]}.
get_multiple(Keys) when is_list(Keys) ->
	get_multiple(Keys, []).

-spec update() ->
	ok.
update() ->
	update(?DEFAULT_CONFIG).

-spec update(Config_file :: string()) ->
	ok.
update(Config_file) ->
	gen_server:cast(?SERVER, {update_config, Config_file}).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

-spec init(Config_file :: string()) ->
	{ok, #state{}}.
init(Config_file) ->
	{ok, Config} = file:consult(Config_file),
	{ok, #state{config = Config}}.

-spec handle_call(Msg :: any(), From :: {pid(), _Tag}, #state{}) ->
	{reply, Reply :: any(), #state{}}.
handle_call({get, Key, Default}, _From, State) ->
	Config = State#state.config,
	Value = from_config(Config, Key, Default),
	{reply, {ok, Value}, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

-spec handle_cast(Msg :: any(), #state{}) ->
	{noreply, #state{}}.
handle_cast({update_config, Config_file}, _State) ->
	{ok, Config} = file:consult(Config_file),
	{noreply, #state{config=Config}};
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(Msg :: any(), #state{}) ->
	{noreply, #state{}}.
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(Reason :: any(), #state{}) ->
	ok.
terminate(_Reason, _State) ->
	ok.

-spec code_change(OldVsn :: any(), #state{}, Extra :: any()) ->
	{ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

from_config(Config, Key, Default) ->
	Path = string:tokens(Key, "."),
	Path2 = lists:map(fun(Val) ->
		list_to_atom(Val)
	end, Path),
	from_config2(Config, Path2, Default).

from_config2(Config, [Key | []], Default) ->
	proplists:get_value(Key, Config, Default);
from_config2(Config, [Path | Rest], Default) ->
	case proplists:get_value(Path, Config) of
		undefined ->
			Default;
		Config2 ->
			from_config2(Config2, Rest, Default)
	end.

%% This is just a wrapper. Maybe later I will implement it on lower level as gen_server callback
get_multiple([], Results) ->
	{ok, lists:reverse(Results)};
get_multiple([Key | Keys], Results) ->
	{Key2, Default} = if
		is_tuple(Key) ->
			Key;
		true ->
			{Key, ?DEFAULT_VALUE}
	end,
	{ok, Result} = get(Key2, Default),
	get_multiple(Keys, [Result | Results]).
