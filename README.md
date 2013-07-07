CfgSrv
========


It is a very simple config server. I bet most of you have already done something similar but maybe this could be useful for someone else.


Usage:

Save your config files with ".config" extension wherever you want. In my case this will be priv/config/dev and priv/config/prod

Example structure of "priv" directory:
![Example tree output for priv directory](http://puu.sh/3wGDa.png "Example tree output for priv directory")

Example config file content (more examples could be found in priv/config directory of the project):

![Example config content](http://puu.sh/3wHns.png "Example config content")

- Starting Cfgsrv

```erlang
cfgsrv:start(),
cfgsrv:set_path("priv/config/dev"). %% Default path when application was started is priv/config
```

- Reading config values

```erlang
cfgsrv:get("app", "key"), %% "Value"
cfgsrv:get("app.key_with_subkeys", "subkey2"). %% subkey2_value
cfgsrv:get("app.key_with_subkeys", "nonexistent_key", my_default_value). %% my_default_value
```

- Reading multiple values from config

```erlang
%% You can also pass tuple {Key, Default_value} to get Default_value instead of 'undefined' when the key was not found
cfgsrv:get_multiple("app.key_with_subkeys.subkey1", ["subsubkey1", {"subsubkey2", default_value}, {"subsubkey3", default_value}]).
%% The result will be [subsubkey1_value, subsubkey2_value, default_value]
```

- Updating configs

```erlang
cfgsrv:update() %% This will update all configs data from the initial :PATH
cfgsrv:update("app") %% This will update only app.config data from the initial :PATH ("priv/config/dev")
```

- Updating configs path

```erlang
cfgsrv:set_path("priv/config/prod") %% This will change path from "dev" to "prod" and load new configs
```
