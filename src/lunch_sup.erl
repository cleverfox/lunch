%%%-------------------------------------------------------------------
%% @doc lunch top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(lunch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,http_childspec/0]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    application:ensure_all_started(cowboy),
    application:ensure_all_started(cowdb),
    {ok, {
       {one_for_all, 5, 10}, 
       [
        {db, {db, start_link, []}, permanent, 5000, worker, []}
       ]++http_childspec()
      } 
    }.

%%====================================================================
%% Internal functions
%%====================================================================
http_childspec() ->
    HTTPDispatch = cowboy_router:compile(
                     [
                      {'_', [
                             {"/api/[...]", apixiom, {lunch_httpapi, #{}}},
                             {"/[...]", cowboy_static,
                              {dir, "public",
                               [
                                {mimetypes, cow_mimetypes, all},
                                {dir_handler, directory_handler}
                               ]
                              }
                             }
                            ]}
                     ]),
    HTTPOpts=[{connection_type, supervisor},
              {port,
               application:get_env(lunch, httpport, 12380)
              }],
    HTTPConnType=#{connection_type => supervisor,
                   env => #{dispatch => HTTPDispatch}},
    HTTPAcceptors=10,
    [
    ranch:child_spec(http,
                     HTTPAcceptors,
                     ranch_tcp,
                     HTTPOpts,
                     cowboy_clear,
                     HTTPConnType),
    ranch:child_spec(http6,
                     HTTPAcceptors,
                     ranch_tcp,
                     [inet6, {ipv6_v6only, true}|HTTPOpts],
                     cowboy_clear,
                     HTTPConnType)
    ].



