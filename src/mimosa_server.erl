-module(mimosa_server).
-export([start/1]).

start(Port) ->
  lager:info("INITIALIZING IRC SERVER"),
  Pid = spawn_link(fun() ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
    spawn(fun() -> acceptor(Listen) end),
    timer:sleep(infinity)
  end),
  {ok, Pid}.

acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
      lager:info("GOT A MESSAGE: ~p", [Msg]),
      handle(Socket)
  end.
