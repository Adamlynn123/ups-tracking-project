-module(store_package_data).
-behaviour(gen_server).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/3,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%===================================================================
%%% API
%%%===================================================================

store(ServerRef, Key, Value) -> 
    gen_server:call(ServerRef, {store, Key, Value}, infinite).


%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init(_Args) ->
        case riakc_pb_socket:start_link("146.190.152.201")  of
            {ok, Riak_pid} -> 
                {ok, Riak_pid};
            _ -> 
                {stop, link_failure}
        end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
handle_call({store, Package_id, Value}, _From, Riak_pid) ->
    
    Request = riakc_obj:new(<<"Package">>, Package_id, Value),

    {reply, riakc_pb_socket:put(Riak_pid, Request), Riak_pid};
    
handle_call(stop, _From, _State) ->
        {stop,normal,
                replace_stopped,
          down}. %% setting the server's internal state to down

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
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
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================




-ifdef(EUNIT).
    -include_lib("eunit/include/eunit.hrl").

setup() -> 
    meck:new(riakc_pb_socket, [passthrough]),
    meck:expect(riakc_pb_socket, start_link, fun(_, _) -> {ok, fake_pid} end),
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

teardown(Pid) ->
    gen_server:stop(Pid),
        meck:unload().

put_error() ->
    meck:expect(riakc_pb_socket, put, fun(_, _) -> {failed, "Put error"} end),
    ?_assertEqual(store_package_data:handle_call({store, "package_id", {[{"holder_id", "time_stamp"}]}}, some_from_pid, some_riak_pid), {reply, {stop, "gen_server stopped"}, some_riak_pid}).

instantiator(Pid) ->
    [?_assertEqual(store_package_data:handle_call({store, "package_id", {[{"holder_id", "time_stamp"}]}}, some_from_pid, some_riak_pid), {reply, ok, some_riak_pid}),
     ?_assertEqual(store_package_data:handle_call({store, "", {[{"holder_id", "time_stamp"}]}}, some_from_pid, some_riak_pid), {reply, {failed, "Empty key"}, some_riak_pid}),
     ?_assertEqual(store_package_data:handle_call({store, "package_id", {[{}]}}, some_from_pid, some_riak_pid), {reply, {failed, "Wrong value"}, some_riak_pid}),
     put_error()
    ].

store_package_data_test_() -> {setup, fun setup/0, fun teardown/1, fun instantiator/1}.

-endif.