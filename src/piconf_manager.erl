%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(piconf_manager).

-include("piconf.hrl").

-export([evalLocalConfig/0, getLocalConfig/0]).

%% @doc evaluate config file, and set application env
%% @end
-spec evalLocalConfig() -> none().
evalLocalConfig() ->
	lager:debug("evaluate local config file"),
	{ok, ConfigFiles} = application:get_env(piconf,configfiles),
	Config = getLocalConfig(ConfigFiles, #config{}),
	[
		[application:set_env(App, Key, Val) || {Key,Val} <- Opts]
		|| {App,Opts} <- Config#config.env
	].


%% @doc returns local config
%% @end
-spec getLocalConfig() -> term().
getLocalConfig() ->
	lager:debug("read local config"),
	{ok, ConfigFiles} = application:get_env(piconf,configfiles),
	getLocalConfig(ConfigFiles, #config{}).

%% @private
%% @doc reads all config from given config file list and returns newest version
%% @end
-spec getLocalConfig([string()], term()) -> term().
getLocalConfig([], Config) ->
	Config;
getLocalConfig(ConfigFiles, Config) ->
	[ConfigFile|ConfigFilesRest] = ConfigFiles,
	NewConfig = case file:consult(ConfigFile) of
		{error, _} ->
			Config;
		{ok, NewConfigIn} ->
			NewConfigIn2 = #config{
					vsn=proplists:get_value(vsn,NewConfigIn,Config#config.vsn),
					piconf=proplists:get_value(piconf,NewConfigIn,[]),
					env=proplists:get_value(env,NewConfigIn,[])
					},
			mergeConfig(Config, NewConfigIn2)
	end,
	getLocalConfig(ConfigFilesRest, NewConfig).

%% @doc compares both configs and merges them, than return a merged config
%% @end
-spec mergeConfig(term(), term()) -> term().
mergeConfig(Config1, Config2) ->
	{Vsn, PiConf, Env} = case Config1#config.vsn > Config2#config.vsn of
		true ->
			{Config1#config.vsn,
			Config1#config.piconf ++ Config2#config.piconf,
			Config1#config.env ++ Config2#config.env};
		false ->
			{Config2#config.vsn,
			Config2#config.piconf ++ Config1#config.piconf,
			Config2#config.env ++ Config1#config.env}
	end,
	#config{
		vsn=Vsn,
		piconf=[{Key, proplists:get_value(Key, PiConf)} || Key <- proplists:get_keys(PiConf)],
		env=[{Key, proplists:get_value(Key, Env)} || Key <- proplists:get_keys(Env)]
		}.

