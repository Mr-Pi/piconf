%%%-------------------------------------------------------------------
%%% @author Mr. Pi <mrpi@mr-pi.de>
%%% @copyright 2013 Mr. Pi
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(piconf_mainServer).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {synced=false}).

-type nodes() :: [node(),...].

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
	lager:debug("~p: start link", [?MODULE]),
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
	lager:debug("~p: init: Opts='[]'", [?MODULE]),
	connect_all_local(),
	getLocalConfig(["/etc/piconf/piconf.config", "/tmp/piconf.config"]),
	net_kernel:monitor_nodes(true),
	State = #state{},
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
handle_call(Request, From, State) ->
	lager:warning("~p: unexpected call: Request='~p', From='~p', State='~p'", [?MODULE, Request, From, State]),
	Reply = ok,
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
	lager:warning("~p: unexpected cast: Msg='~p', State='~p'", [?MODULE, Msg, State]),
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
	lager:info("~p: lost node ~p", [?MODULE, Node]),
	{noreply, State};
handle_info({nodeup, Node}, State) ->
	lager:info("~p: added node ~p", [?MODULE, Node]),
	{noreply, State};
handle_info(Info, State) ->
	lager:warning("~p: unexpected info: Info='~p', State='~p'", [?MODULE, Info, State]),
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
	lager:debug("~p: terminate: Reason='~p', State='~p'", [?MODULE, Reason, State]),
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
	lager:notice("~p: code change: OldVsn='~p', State='~p', Extra='~p'", [?MODULE, OldVsn, State, Extra]),
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% @private
%% @doc connects to all available local nodes
%% @end
-spec connect_all_local() -> true.
connect_all_local() ->
	lager:debug("~p: function call: connect_all_local()", [?MODULE]),
	{ok, NodesPrefixIn} = net_adm:names(),
	Domains = [ lists:last(string:tokens(atom_to_list(node()),[$@])) | [net_adm:localhost()] ],
	NodesPrefix = lists:map(fun(X) -> {NodePrefix,_}=X, NodePrefix end, NodesPrefixIn),
	Nodes = [ list_to_atom(NodePrefix ++ "@" ++ Domain) || NodePrefix <- NodesPrefix, Domain <- Domains ],
	ping_nodes(Nodes).
	

%% @private
%% @doc ping(connect) nodes
%% @end
-spec ping_nodes(nodes() | []) -> true.
ping_nodes(Nodes) ->
	lager:debug("~p: function call: ping_nodes(Nodes)", [?MODULE]),
	is_list(net_adm:ping_list(Nodes)).


%% @private
%% @doc read config file, and valid version number
%% @end
-spec getLocalConfig([string()]) -> term().
getLocalConfig(ConfigFiles) ->
	lager:debug("~p: function call: getLocalConfig(ConfigFiles)", [?MODULE]),
	getLocalConfig(ConfigFiles, []).

%% @private
-spec getLocalConfig([string()], term()) -> term().
getLocalConfig([], Config) ->
	lager:debug("~p: function call: getLocalConfig([], Config)", [?MODULE]),
	Config;
%% @private
getLocalConfig(ConfigFiles, Config) ->
	lager:debug("~p: function call: getLocalConfig(ConfigFiles, Config)", [?MODULE]),
	[ConfigFile|ConfigFilesRest] = ConfigFiles,
	getLocalConfig(ConfigFilesRest, [ConfigFile|Config]).
