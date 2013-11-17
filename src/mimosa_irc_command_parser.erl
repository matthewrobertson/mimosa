-module(mimosa_irc_command_parser).
-export([parse/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

chop(Command) -> binary:replace(Command, <<"\r\n">>, <<"">>).

parse(<<"PING ", Suffix/binary>>) ->
  {ping, chop(Suffix)};
parse(<<"PRIVMSG ", Suffix/binary>>) ->
  {privmsg, chop(Suffix)};
parse(<<"ERROR ", Suffix/binary>>) ->
  {error, chop(Suffix)};
parse(<<":", Suffix/binary>>) ->
  % drop everything up to and including the first ' ' and parse the rest
  [_, Rest] = binary:split(Suffix, <<" ">>),
  parse(Rest);
parse(Command) -> {unknown, Command}.



-ifdef(TEST).

ping_test() ->
  Command = <<"PING :hubbard.freenode.net\r\n">>,
  ?assertEqual({ping, <<":hubbard.freenode.net">>}, parse(Command)),
  ok.

privmsg_test() ->
  Command = <<":matthew__!~textual@S0106586d8f63ed2e.ok.shawcable.net PRIVMSG matthew_abc :yo\r\n">>,
  ?assertEqual({privmsg, <<"matthew_abc :yo">>}, parse(Command)),
  ok.

error_test() ->
  Command = <<"ERROR :Closing Link: S0106602ad072800b.vc.shawcable.net (Ping timeout: 264 seconds)\r\n">>,
  ?assertEqual({error, <<":Closing Link: S0106602ad072800b.vc.shawcable.net (Ping timeout: 264 seconds)">>}, parse(Command)),
  ok.

unknown_test() ->
  Command = <<"Some random stuff\r\n">>,
  ?assertEqual({unknown, Command}, parse(Command)),
  ok.

-endif.
