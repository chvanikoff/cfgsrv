CfgSrv
========


It is a very simple config server. I bet most of you have already done something similar but maybe this could be useful for someone else.


Usage:


```
{ok, Pid} = cfgsrv:start_link("path_to_config"),
{ok, Val} = cfgsrv:get(key). %% atoms are also allowed ;)
```


Features:

- read config values from config file

```
{ok, Value} = cfgsrv:get("some.key"),

{ok, Value2} = cfgsrv:get("some.key2", "this is default value (which is 'undefined' by default)").
```
- read multiple values from config file

```
{ok, [Value1, Value2]} = cfgsrv:get(["some.key1", "some.key2"]),

{ok, [{Value1, "default value for this key"}, Value2]} = cfgsrv:get(["some.key1", "some.key2"]).
```
- replace config file

```
ok = cfgsrv:update("another_cfg.config").
```
