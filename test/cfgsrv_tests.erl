-module(cfgsrv_tests).
-author('chvanikoff <chvanikoff@gmail.com>').
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-spec setup() ->
	ok.
setup() ->
	application:start(cfgsrv),
	cfgsrv:set_path("../priv/config/dev").

teardown(_) ->
	application:stop(cfgsrv).

-spec cfgsrv_test_() ->
	Generator :: any().
cfgsrv_test_() ->
	{setup,
		fun setup/0,
		fun teardown/1,
		{inorder, [
			?_test(?assertEqual(
				"Value",
				cfgsrv:get("app", "key")
			)),
			?_test(?assertEqual(
				subsubkey2_value,
				cfgsrv:get("app.key_with_subkeys.subkey1", "subsubkey2")
			)),
			?_test(?assertEqual(
				["127.0.0.1", 8087, nonexistent],
				cfgsrv:get_multiple("riak", ["host", "port", {"nonexistent.key", nonexistent}])
			)),
			?_test(?assertEqual(
				ok,
				cfgsrv:set_path("../priv/config/prod")
			)),
			?_test(?assertEqual(
				"prod_Value",
				cfgsrv:get("app", "key")
			)),
			?_test(?assertEqual(
				undefined,
				cfgsrv:get("app", "nonexistent.key")
			)),
			?_test(?assertEqual(
				mydefaultval,
				cfgsrv:get("app", "nonexistent.key", mydefaultval)
			)),
			?_test(?assertEqual(
				ok,
				cfgsrv:set_path("unexistent/path")
			)),
			?_test(?assertEqual(
				"prod_Value",
				cfgsrv:get("app", "key")
			)),
			?_test(?assertEqual(
				ok,
				cfgsrv:set_path("../priv/config/dev")
			)),
			?_test(?assertEqual(
				["127.0.0.1", 8087, nonexistent],
				cfgsrv:get_multiple("riak", ["host", "port", {"nonexistent.key", nonexistent}])
			))
		]}}.