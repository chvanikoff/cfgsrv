-module(cfgsrv_tests).
-author('chvanikoff <chvanikoff@gmail.com>').
-include_lib("eunit/include/eunit.hrl").
-include("../src/cfgsrv.hrl").
-compile(export_all).

-spec setup() ->
	ok.
setup() ->
	{ok, _Pid} = cfgsrv:start_link("../priv/application.config").

-spec cfgsrv_test_() ->
	Generator :: any().
cfgsrv_test_() ->
	{setup,
		fun setup/0,
		{inorder, [
			?_test(?assertEqual(
				{ok, lorem},
				cfgsrv:get("somekey1.subkey1_1")
			)),
			?_test(?assertEqual(
				{ok, "ipsum"},
				cfgsrv:get('somekey1.subkey1_2')
			)),
			?_test(?assertEqual(
				{ok, [123, 789, nonexistent, 456]},
				cfgsrv:get_multiple(["somekey2.subkey2_1", "somekey2.subkey2_3", {"nonexistent.key", nonexistent}, "somekey2.subkey2_2"])
			)),
			?_test(?assertEqual(
				ok,
				cfgsrv:update("../test/test_config.config")
			)),
			?_test(?assertEqual(
				{ok, customlorem},
				cfgsrv:get("customkey1.subcustomkey1_1")
			)),
			?_test(?assertEqual(
				{ok, ?DEFAULT_VALUE},
				cfgsrv:get("nonexistent.key")
			)),
			?_test(?assertEqual(
				{ok, mydefaultval},
				cfgsrv:get("nonexistent.key", mydefaultval)
			))
		]}}.