%%%-------------------------------------------------------------------
%% @doc  public API
%% @end
%%%-------------------------------------------------------------------

-module(ups_project_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
	    {'_', [
	        {"/", toppage_h, []},
		{"/store_package_info",store_package_h,[]},
        {"/query_package_history",query_history_h,[]}
	    ]}
	]),

	PrivDir = code:priv_dir(db_access),
        {ok,_} = cowboy:start_tls(https_listener, [
                  		{port, 443},
				{certfile, PrivDir ++ "/ssl/fullchain.pem"},
				{keyfile, PrivDir ++ "/ssl/privkey.pem"}
              		], #{env => #{dispatch => Dispatch}}),
	db_access_sup:start_link().
stop(_State) ->
    ok.

%% internal functions
