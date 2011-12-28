%%%-------------------------------------------------------------------
%%% @author Ward Bekker <>
%%% @copyright (C) 2011, Ward Bekker
%%% @doc
%%%
%%% @end
%%% Created : 23 Dec 2011 by Ward Bekker <>
%%%-------------------------------------------------------------------
-module(scraper_worker_ser).

-compile({parse_transform, do}).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(TIMEOUT, 1000).

-record(state, {}).

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
    gen_server:start_link(?MODULE, [], []).

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
    {ok, #state{}, ?TIMEOUT}.

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
handle_call(_Request, _From, State) ->
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
handle_cast(_Msg, State) ->
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
%% handle_info(_Info, State) ->
%%     {noreply, State};

handle_info(timeout, State) ->
    do_work(),
    {noreply, State, ?TIMEOUT};
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_work() ->
    do([error_m ||
	   Url <- scraper_queue_ser:get_url(),
	   Document <- get_document(Url),
	   write_document(Document)
       ]).

get_document(Url) ->
    Response = httpc:request(get, {Url, []}, [], []),
    case Response of 
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
	    error_m:return(Body);
	_ ->
	    error_m:fail("error fetching document")
    end.

get_hyperlinks(Document) ->
    %% "href=\"(.*?)\""
    foo.

write_document(Document) ->
    Guid = os:cmd("uuidgen"),
    do([error_m || 
	   Hdl <- file:open(lists:concat(["/tmp/", "scraper_", Guid ]), [write]),
	   Result <- return(do([error_m || 
				  file:write(Hdl, Document),
				  file:sync(Hdl)])),
	   file:close(Hdl),
	   Result]).
	   
				  
	   
		     
