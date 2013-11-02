%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%% Callback module for piconf (shared config application)
%%% @end
%%%-------------------------------------------------------------------
-module(piconf).

-export([reboot/0]).


%% @doc reboots all nodes
%% @end
-spec reboot() -> no_return().
reboot() ->
	lager:debug("~p: function call: reboot()", [?MODULE]),
	lists:map(fun(X) -> {piconf_mainServer, X} ! reboot end, nodes()),
	erlang:send_after(20, piconf_mainServer, reboot).
