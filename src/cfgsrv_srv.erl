-module(cfgsrv_srv).
-author('chvanikoff <chvanikoff@gmail.com>').

%% Gen_server behaviour
-behaviour(gen_server).
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
	start_link/0, start_link/1
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_CONFIG_PATH, "priv/config").

-record(state, {
	configs,
	root_dir,
	path
}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	start_link(?DEFAULT_CONFIG_PATH).


start_link(Path) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Path, []).

%% ===================================================================
%% Gen_server callbacks
%% ===================================================================

init(Path) ->
	ets:new(cfgsrv, [set, named_table, {read_concurrency, true}]),
	Configs = load_configs(Path),
	{ok, #state{configs = Configs, path = Path}}.


handle_call({get, Path, Key, Default}, _From, State = #state{configs = Configs}) ->
	Data = from_configs(Configs, Path, Key, Default),
	ets:insert(cfgsrv, {{Path,Key,Default}, Data}),
	{reply, from_configs(Configs, Path, Key, Default), State};

handle_call(test, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.


handle_cast({update_config, undefined}, #state{path = Path} = State) ->
	Configs = load_configs(Path),
	{noreply, State#state{configs = Configs}};

handle_cast({update_config, File}, #state{path = Path, configs = Configs} = State) ->
	case file:consult(Path ++ "/" ++ File ++ ".config") of
		{ok, Config} ->
			Key = list_to_atom(filename:basename(File, ".config")),
			{noreply, State#state{configs = lists:keyreplace(Key, 1, Configs, {Key, Config})}};
		_ -> {noreply, State}
	end;

handle_cast({set_path, Path}, State) ->
	case filelib:is_dir(Path) of
		true ->
			Configs = load_configs(Path),
			{noreply, State#state{path = Path, configs = Configs}};
		_ -> {noreply, State}
	end;

handle_cast(_Msg, State) ->
	{noreply, State}.


handle_info(_Info, State) ->
	{noreply, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

load_configs(Path) ->
	load_configs(filelib:wildcard(Path ++ "/*.config"), []).


load_configs([], Acc) -> 
	ets:delete_all_objects(cfgsrv),
	Acc;
load_configs([File | Files], Acc) ->
	case file:consult(File) of
		{ok, [Config]} ->
			New_cfg = get_cfg(File, Config),
			load_configs(Files, [New_cfg | Acc]);
		_ -> load_configs(Files, Acc)
	end.


get_cfg(File, Config) ->
	Config2 = case lists:keyfind(extends, 1, Config) of
		false -> Config;
		{_, Path} ->
			Ext = lists:concat([filename:dirname(File), "/", Path]),
			Ext2 = case filelib:is_dir(Ext) of
				true -> Ext ++ "/" ++ filename:basename(File);
				_ -> Ext
			end,
			case file:consult(Ext2) of
				{ok, [Ext_cfg]} ->
					dict:to_list(dict:merge(fun(_, V, _) -> V end, dict:from_list(Config), dict:from_list(Ext_cfg)));
				_ -> Config
			end
	end,
	{list_to_atom(filename:basename(File, ".config")), Config2}.


from_configs(Configs, Path, Key, Default) ->
	Path2 = lists:map(fun(Val) ->
		list_to_atom(Val)
	end, string:tokens(Path, ".") ++ [Key]),
	from_config2(Configs, Path2, Default).


from_config2(Configs, [Key | []], Default) ->
	case lists:keyfind(Key, 1, Configs) of
		{Key, Val} -> Val;
		_ -> Default
	end;

from_config2(Config, [Path | Rest], Default) ->
	case lists:keyfind(Path, 1, Config) of
		{Path, Config2} -> from_config2(Config2, Rest, Default);
		_ -> Default
	end.
