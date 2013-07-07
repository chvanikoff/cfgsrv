-module(cfgsrv).
-author('chvanikoff <chvanikoff@gmail.com>').

%% API
-export([
	start/0,
	get/2, get/3,
	get_multiple/2,
	update/0, update/1,
	set_path/1,
	test/0
]).
-define(SERVER, cfgsrv_srv).

%% Application behaviour
-behaviour(application).
-export([start/2, stop/1]).

%% Supervisor behaviour
-behaviour(supervisor).
-export([init/1]).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
	ensure_started([?MODULE]).

get(Path, Key) ->
	get(Path, Key, undefined).


get(Path, Key, Default) ->
	gen_server:call(?SERVER, {get, Path, Key, Default}).


get_multiple(Path, Keys) ->
	get_multiple(Path, Keys, []).


update() ->
	update(undefined).


update(Config_file) ->
	gen_server:cast(?SERVER, {update_config, Config_file}).


set_path(Path) ->
	gen_server:cast(?SERVER, {set_path, Path}).


test() -> gen_server:call(?SERVER, test).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
	ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Restart_strategy = {one_for_one, 5, 10},
	Children = [
		?CHILD(cfgsrv_srv, worker)
	],
	{ok, {Restart_strategy, Children}}.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Rest] = Apps) ->
	case application:start(App) of
		ok -> ensure_started(Rest);
		{error, {already_started, App}} -> ensure_started(Rest);
		{error, {not_started, Dependency}} -> ensure_started([Dependency | Apps])
	end.

%% This is just a wrapper. Maybe later I will implement it on lower level as gen_server callback
get_multiple(_Path, [], Results) ->
	lists:reverse(Results);

get_multiple(Path, [Key | Keys], Results) ->
	{Key2, Default} = if
		is_tuple(Key) -> Key;
		true -> {Key, undefined}
	end,
	get_multiple(Path, Keys, [get(Path, Key2, Default) | Results]).

