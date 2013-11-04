%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%% Callback module for piconf (shared config application)
%%% @end
%%%-------------------------------------------------------------------
-module(piconf).

-export([reboot/0,get/1,get/2]).

-define(NYI, lager:debug("not yet implemented"), throw(not_yet_implemented)).

%% @doc get global config value
%% @end
-spec get(term()) -> term() | undefined.
get(Key) ->
	?NYI.

%% @doc get config value
%% @end
-spec get(atom(), term()) -> term() | undefined.
get(App, Key) ->
	?NYI.

%% @doc reboots all nodes
%% @end
-spec reboot() -> no_return().
reboot() ->
	lager:debug("function call: reboot()"),
	lists:map(fun(X) -> {piconf_mainServer, X} ! reboot end, nodes()),
	erlang:send_after(20, piconf_mainServer, reboot).
