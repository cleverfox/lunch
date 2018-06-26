-module(lunch_httpapi).

-export([h/3, after_filter/1]).

-export([answer/0, answer/1, err/1, err/2, err/3, err/4]).


err(ErrorCode) ->
    err(ErrorCode, <<"">>, #{}, #{}).

err(ErrorCode, ErrorMessage) ->
    err(ErrorCode, ErrorMessage, #{}, #{}).

err(ErrorCode, ErrorMessage, Data) ->
    err(ErrorCode, ErrorMessage, Data, #{}).

err(ErrorCode, ErrorMessage, Data, Options) ->
    Required0 =
        #{
            <<"result">> => false,
            <<"code">> => ErrorCode,
            <<"msg">> => ErrorMessage
        },

    answer_formater(
        maps:get(http_code, Options, 200),
        maps:merge(Data, Required0)
    ).

answer() ->
    answer(#{}).

answer(Data) ->
    answer(Data, #{}).

answer(Data, _Options) when is_map(Data) ->
   answer_formater(
        200,
        maps:put(result, true, Data)
    ).

answer_formater(HttpStatus, Data)
    when is_integer(HttpStatus) andalso is_map(Data) ->
    {HttpStatus, Data}.



after_filter(Req) ->
  Origin=cowboy_req:header(<<"origin">>, Req, <<"*">>),
  Req1=cowboy_req:set_resp_header(<<"access-control-allow-origin">>,
                                  Origin, Req),
  Req2=cowboy_req:set_resp_header(<<"access-control-allow-methods">>,
                                  <<"GET, POST, OPTIONS">>, Req1),
  Req4=cowboy_req:set_resp_header(<<"access-control-max-age">>,
                                  <<"86400">>, Req2),
  cowboy_req:set_resp_header(<<"access-control-allow-headers">>,
                             <<"content-type">>, Req4).

h(Method, [<<"api">>|Path], Req) ->
  lager:info("Path ~p", [Path]),
  h(Method, Path, Req);

h(<<"GET">>, [<<"list">>], _Req) ->
  {ok, L}=gen_server:call(db, list),
  answer(
    #{ data => L }
   );

h(<<"POST">>, [<<"dinner">>, <<"adduser">>], Req) ->
  #{<<"username">>:=User}=apixiom:bodyjs(Req),
  case gen_server:call(db, {add_user, User}) of
    {ok, U} ->
      answer( #{ data => U });
    {exists, U} ->
      answer( #{ data => U })
  end;

h(<<"POST">>, [<<"dinner">>, <<"add">>], Req) ->
%  lager:info("B ~p",[apixiom:bodyjs(Req)]),
  #{<<"raw">>:=R}=apixiom:bodyjs(Req),
  L=lists:filtermap(
    fun(#{<<"sum">> := Sum,
          <<"sumReal">> := Payed,
          <<"userId">> := UID}
       ) ->
        {true, {binary_to_integer(UID), Sum, Payed}};
       (_) -> false
    end, R),
  {TotalE,TotalP}=lists:foldl(
      fun({_UID, Eated, Payed}, {TE, TP}) ->
          {TE+Eated, TP+Payed}
      end, {0,0}, L),
  lager:info("Eated ~p Payed ~p",[TotalE, TotalP]),
  if(TotalE=/=TotalP) ->
      err(1,<<"Lunch cost and payed amount are different">>);
    true ->
      lists:foreach(fun(E) ->
                        lager:info("L ~p",[E])
                    end, L),
      answer(
        #{ data => gen_server:call(db, {lunch, L}) })
  end;

h(<<"GET">>, [<<"users">>], _Req) ->
  {ok, Users} = gen_server:call(db, users),
  answer(
    #{
       data => Users
    }).

