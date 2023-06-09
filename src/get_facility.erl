%%%=============================================================================
%%% @doc get_facility
%%% @end
%%%=============================================================================
-module(get_facility).
-behaviour(gen_server).

%% API
-export([start_link/3, stop/1, get/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

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
-spec start_link(atom(), atom(), atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Registration_type, Name, Args) ->
  gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Name :: atom()) -> {ok} | {error, term()}.
stop(Name) ->
  gen_server:call(Name, stop).

get(_, Vehicle_uuid) when is_list(Vehicle_uuid) == false; Vehicle_uuid == [] ->
  {error, "Key must be a non-empty string."};
get(ServerRef, Vehicle_uuid) ->
  gen_server:call(ServerRef, {get, Vehicle_uuid}, infinity).

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
-spec init(term()) -> {ok, term()} | {ok, term(), number()} | ignore | {stop, term()}.
init([]) ->
  case riakc_pb_socket:start_link(env:riak_address(), 8087, [{connect_timeout, 1000}]) of
    {ok, Riak_pid} -> {ok, Riak_pid};
    _ -> {stop, link_failure}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: pid(), State :: term()) ->
                   {reply, term(), term()} |
                   {reply, term(), term(), integer()} |
                   {noreply, term()} |
                   {noreply, term(), integer()} |
                   {stop, term(), term(), integer()} |
                   {stop, term(), term()}.
handle_call(stop, _From, _State) ->
  {stop, normal, replace_stopped, down}; %% setting the server's internal state to down
handle_call({get, Vehicle_uuid}, _From, Riak_pid) ->
  case riakc_pb_socket:get(Riak_pid, <<"facility">>, term_to_binary(Vehicle_uuid)) of
    {ok, Fetched} ->
      {reply, binary_to_term(riakc_obj:get_value(Fetched)), Riak_pid};
    {error, notfound} -> {reply, [], Riak_pid};
    Error ->
      {stop, Error, Error, down}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: term()) ->
                   {noreply, term()} | {noreply, term(), integer()} | {stop, term(), term()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info :: term(), State :: term()) ->
                   {noreply, term()} | {noreply, term(), integer()} | {stop, term(), term()}.
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
-spec terminate(Reason :: term(), term()) -> term().
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

% tests for the get_facility:handle_call callback for get/3.
handle_call_get_test_() ->
  {setup, fun setup/0, fun cleanup/1, fun instantiator/1}.

setup() ->
  meck:new(riakc_pb_socket, [passthrough]),
  meck:new(riakc_obj, [passthrough]),
  ok.

cleanup(_) ->
  meck:unload().

instantiator(_) ->
  [
   handle_call_get_happy(),
   handle_call_get_fetch_error()
  ].

handle_call_get_happy() ->
  meck:expect(riakc_pb_socket, get, fun(_, _, _) -> {ok, ok} end),
  meck:expect(riakc_obj,
  get_value,
  fun(_) ->
     <<131,107,0,13,78,101,119,32,89,111,114,107,32,67,105,116,121>>
  end),

  Expected = {reply, "New York City", some_riak_pid},

  Actual1 = get_facility:handle_call({get, "key1"}, some_from_pid, some_riak_pid),

  Test1 = ?_assertEqual(Expected, Actual1),

  [Test1].

handle_call_get_fetch_error() ->
  meck:expect(riakc_pb_socket, get, fun(_, _, _) -> {error, "Error message"} end),

  Expected = {error, {error, "Error message"}, down},

  Actual1 = get_facility:handle_call({get, "key1"}, some_from_pid, some_riak_pid),

  Test1 =
    {"Handle case if riakc_pb_socket:get returns an error.",
     ?_assertEqual(Expected, Actual1)},

  [Test1].

% tests for the get_facility:get/2 api function.
get_test_() ->
  [
   get_invalid_key()
  ].

get_invalid_key() ->
  Expected = {error, "Key must be a non-empty string."},

  Actual1 = catch get_facility:get(pid, not_a_string),
  Actual2 = catch get_facility:get(pid, ""),

  Test1 = ?_assertEqual(Expected, Actual1),
  Test2 = ?_assertEqual(Expected, Actual2),

  [Test1, Test2].

-endif.