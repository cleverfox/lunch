%%%-------------------------------------------------------------------
%% @doc db gen_server
%% @end
%%%-------------------------------------------------------------------
-module(db).
-author("cleverfox <devel@viruzzz.org>").
-create_date("2018-06-26").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Pid} = cowdb:open("lunch.db"),
  cowdb:put(Pid, {extra, last_run}, os:system_time(seconds)),
  {ok, #{
     args => Args,
     db => Pid
    }
  }.

handle_call(users, _From, #{db:=DB}=State) ->
  UserList=case cowdb:get(DB, users) of
             not_found -> [];
             {ok,{users,L2}} -> L2
           end,
  Users=[ case cowdb:get(DB, {user,X}) of
             not_found -> false;
             {ok,{{user,X},Descr}} -> Descr
           end || X<-UserList],
  {reply, {ok, Users}, State};

handle_call(list, _From, #{db:=DB}=State) ->
  P=case cowdb:get(DB, last_lunch) of
      not_found -> 0;
      {ok,{last_lunch,L2}} -> L2
    end,
  {reply, {ok, pull(DB, P, P-(86400*7*5), [])}, State};


handle_call({add_user, Username}, _From, #{db:=DB}=State) when is_binary(Username) ->
  lager:info("U ~p",[cowdb:get(DB, users_id_seq)]),
  UserID=case cowdb:get(DB, users_id_seq) of
             not_found -> 1;
           {ok,{users_id_seq,L1}} -> L1+1
           end,
  UserList=case cowdb:get(DB, users) of
             not_found -> [];
             {ok,{users,L2}} -> L2
           end,
  Found=lists:foldl(
    fun(UID, false) ->
        {ok, {{user, UID}, #{name:=Username1}=B}}=cowdb:get(DB, {user, UID}),
            if Username == Username1 ->
                 {true, B};
               true ->
                 false
            end;
       (_, {true,_}=Acc) ->
        Acc
    end, false, UserList),
  case Found of 
    {true, FUser} ->
       {reply, {exists, FUser}, State};
    false ->
       User=#{ id=>UserID, name=>Username, balance=>0},
       cowdb:put(DB, users_id_seq, UserID),
       cowdb:put(DB, {user, UserID}, User),
       cowdb:put(DB, users, [UserID|UserList]),
       {reply, {ok, User}, State}
  end;

handle_call({lunch, List}, _From, #{db:=DB}=State) ->
  L1=lists:map(fun({UserID, Eated, Payed}) ->
                   {ok, {{user, UserID}, #{balance:=B}=UInfo}}=cowdb:get(DB,{user, UserID}),
                   {UserID, UInfo#{balance=>B+Payed-Eated}}
                end, List),
  T=os:system_time(seconds),
  P=case cowdb:get(DB, last_lunch) of
             not_found -> 0;
             {ok,{last_lunch,L2}} -> L2
           end,

  cowdb:transact(DB, [
                      {add, {lunch, T}, #{items=>List, t=>T, pre=>P}},
                      {add, last_lunch, T}
                      |
                      lists:map(fun({UserID, U1}) ->
                                    {add, {user, UserID}, U1}
                                end, L1)
                     ]),
  {reply, #{ok=>true, t=>T}, State};

handle_call(_Request, _From, State) ->
    lager:notice("Unknown call ~p",[_Request]),
    {reply, unhandled, State}.

handle_cast(_Msg, State) ->
    lager:notice("Unknown cast ~p",[_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:notice("Unknown info  ~p",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

pull(DB, T, FromTime, Acc) ->
  L=cowdb:get(DB,{lunch, T}),
  lager:info("Getting lunch ~p: ~p",[T, L]),
  case L of
    {ok, {{lunch, T}, #{pre:=P}=E}} when P<FromTime ->
      [prettify_log(DB,E)|Acc];
    {ok, {{lunch, T}, #{pre:=P}=E}} when P=/=T ->
      pull(DB, P, FromTime, [prettify_log(DB,E)|Acc]);
    _ ->
      Acc
  end.

prettify_log(DB,#{items:=Is,t:=T}) ->
  L1=lists:map(fun({UID, Eated, Payed}) ->
                User=case cowdb:get(DB, {user, UID}) of
                  {ok, {{user, UID}, #{name:=Username}}} ->
                    Username;
                  _ ->
                    UID
                end,
                #{u=>User,
                  e=>Eated,
                  p=>Payed
                 }
            end, Is),
  #{t=>T,items=>L1}.
