%%%-------------------------------------------------------------------
%% @doc ups_project top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ups_project_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 0,
                 period => 1},

                 Sp_sup = generate_spec(store_package_sup, supervisor, store_package_sup, []),
                 Sv_sup = generate_spec(store_vehicle_sup, supervisor, store_vehicle_sup, []),
                 Sf_sup = generate_spec(store_facility_sup, supervisor, store_facility_sup, []),
                 Gp_sup = generate_spec(get_package_sup, supervisor, get_package_sup, []),
                 Gv_sup = generate_spec(get_vehicle_sup, supervisor, get_vehicle_sup, []),
                 Gf_sup = generate_spec(get_facility_sup, supervisor, get_facility_sup, []),

    ChildSpecs = [Sp_sup, Sv_sup, Sf_sup, Gp_sup, Gv_sup, Gf_sup],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

generate_spec(Module,Type, Name, State)->
%%
%% A child Specification is a record with the following mappings.
%%
%% child_spec() = #{id => child_id(),       % mandatory. The name to be registered.
%%                  start => mfargs(),      % mandatory. The module's startup function.
%%                  restart => atom(),              % optional. Options are permanent (restart always), transient (restart only after abnormal termination), and temporary (never restart).
%%                  shutdown => integer()|atom(),   % optional. A number or the atom infinity representing the milliseconds allowed for a soft, normal shutdown before it is killed brutally.
%%                  type => atom(),                 % optional. Options are worker or supervisor.
%%                  modules => [module()]|atom()}   % optional. A list of modules to be considered for upgrading
%%                                                  % when the child's code is upgraded. The dynamic atom is used for when 
%%                                                  % such a list is unknown, for example when the child is a 
%%                                                  % gen_event manager with some unknown types of gen_event handler
%%                                                  % modules to be added later.  
        #{id => Name,
            start => {Module,start_link,[local, Name, State]},
            restart => permanent,
            shutdown => 2000,
            type => Type,
            modules => [Module]}.