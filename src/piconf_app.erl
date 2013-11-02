%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(piconf_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
	 stop/1]).

-define(DEPS, [lager]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	lists:foreach(fun(Dep) ->
		ok = application:ensure_started(Dep) end, ?DEPS),
	{ok, ConfigFiles} = application:get_env(piconf,configfiles),
	piconf_manager:evalLocalConfig(ConfigFiles),
	lists:foreach(fun(Dep) ->
		application:stop(Dep), application:start(Dep) end, ?DEPS),
	piconf_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
	ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


