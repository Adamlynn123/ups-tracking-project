%% @doc A handler to store the vehicle of data in the database.
-module(store_vehicle_h).

-export([init/2]).

init(Req0, Opts) ->

	{ok,Data,_} = cowboy_req:read_body(Req0),

    % Message
	% #{<<"package_uuid">> := Pack, <<"holder_uuid">> := Holder, <<"time_stamp">> := Time_stamp} = jsx:decode(Data),
	
	% Package_uuid:binary_to_list(Pack),
	% Holder_uuid:binary_to_list(Holder),
	% List = get_package:get(Package_uuid), 
    % store_package_data:store(rr_distributor:get(), Package_uuid, [{Holder_uuid, Time_stamp} | List]),

	[Vehicle_uuid, Values|_] = jsx:decode(Data),
	store_package_data:store(rr_distributor:get(), Vehicle_uuid, Values),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/json">>
	}, "[\"done\"]", Req0),
	{ok, Req, Opts}.
