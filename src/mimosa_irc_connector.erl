-module(mimosa_irc_connector).

-export([connect/2]).

connect(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active,true}]),
  gen_tcp:send(Socket, "Nick matthew_1"),
  receive
    Message ->
      io:format("Got a message: ~w~n", [Message])
  end.
