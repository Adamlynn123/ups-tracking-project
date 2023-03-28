%% @doc A handler to store the friends of some person in the database.
-module(store_package_h).

-export([init/2]).

init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),

    % Message
	#{<<"package_uuid">> := Pack, <<"holder_uuid">> := Holder, <<"time_stamp">> := Time_stamp} = jsx:decode(Data),
	
	Package_uuid = binary_to_list(Pack),
	Holder_uuid = binary_to_list(Holder),
	List = get_package:get(Package_uuid), 
    store_package_data:store(rr_distributor:get(store_package_rr), Package_uuid, [{Holder_uuid, Time_stamp} | List]),

	% store(ServerRef Pid, Package_id, [{Holder_id, Timestamp}])
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.
