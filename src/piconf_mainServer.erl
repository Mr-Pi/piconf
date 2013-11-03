%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(piconf_mainServer).

-behaviour(gen_server).

-include("piconf.hrl").

-define(NODE_RECONNECT_TIME, 10000).
-define(NODE_RECONNECT_COUNT, 500).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {synced=false, vsn=0, piconf=[]}).

%-type nodes() :: [node(),...].

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	lager:debug("start link"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	lager:debug("init: Opts='[]'"),
	connect_all_local(),
	Config = piconf_manager:getLocalConfig(),
	State = #state{vsn=Config#config.vsn, piconf=Config#config.piconf},
	[erlang:spawn_link(fun() ->
		try_reconnect(?NODE_RECONNECT_TIME, ?NODE_RECONNECT_COUNT, Node) end)
		|| Node <- proplists:get_value(nodes,State#state.piconf,[])],
	net_kernel:monitor_nodes(true),
	{ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(test, From, State) ->
	lager:warning("test call: From='~p', State='~p'", [From, lager:pr(State,?MODULE)]),
	{noreply, State};
handle_call(Request, From, State) ->
	lager:warning("unexpected call: Request='~p', From='~p', State='~p'", [Request, From, lager:pr(State,?MODULE)]),
	Reply = unexpected,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
	lager:warning("unexpected cast: Msg='~p', State='~p'", [Msg, lager:pr(State,?MODULE)]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(reboot, State) ->
	lager:notice("reboot"),
	halt(),
	{noreply, State};
handle_info({nodedown, Node}, State) ->
	lager:info("lost node ~p", [Node]),
	case lists:member(Node, proplists:get_value(nodes, State#state.piconf,[])) of
		true -> erlang:spawn_link(fun() -> try_reconnect(?NODE_RECONNECT_TIME,?NODE_RECONNECT_COUNT,Node) end);
		_ -> false
	end,
	{noreply, State};
handle_info({nodeup, Node}, State) ->
	lager:info("added node ~p", [Node]),
	{noreply, State};
handle_info(Info, State) ->
	lager:warning("unexpected info: Info='~p', State='~p'", [Info, lager:pr(State,?MODULE)]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	lager:debug("terminate: Reason='~p', State='~p'", [Reason, lager:pr(State,?MODULE)]),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
	lager:notice("code change: OldVsn='~p', State='~p', Extra='~p'", [OldVsn, lager:pr(State,?MODULE), Extra]),
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
%% @doc connects to all available local nodes
%% @end
-spec connect_all_local() -> true.
connect_all_local() ->
	lager:debug("connect all local nodes"),
	{ok, NodesPrefixIn} = net_adm:names(),
	Domains = [ lists:last(string:tokens(atom_to_list(node()),[$@])) | [net_adm:localhost()] ],
	NodesPrefix = lists:map(fun(X) -> {NodePrefix,_}=X, NodePrefix end, NodesPrefixIn),
	Nodes = [ list_to_atom(NodePrefix ++ "@" ++ Domain) || NodePrefix <- NodesPrefix, Domain <- Domains ],
	net_adm:ping_list(Nodes).
	

%% @doc try all Time ms, to reconnect Node
%% @end
-spec try_reconnect(pos_integer(), non_neg_integer(), node()) -> none().
try_reconnect(_Time, 0, Node) ->
	lager:debug("try reconnect node ~p", [Node]),
	case net_adm:ping(Node) of
		pong -> lager:info("reconnect node ~p successful", [Node]);
		pang -> lager:error("failed to reconnect node ~p", [Node])
	end;
try_reconnect(Time, Count, Node) ->
	lager:debug("try reconnect node ~p", [Node]),
	case net_adm:ping(Node) of
		pong -> lager:info("reconnect node ~p successful", [Node]);
		pang ->
			lager:debug("remaining retries ~p", [Count-1]),
			timer:sleep(Time),
			try_reconnect(Time, Count-1, Node)
	end.
