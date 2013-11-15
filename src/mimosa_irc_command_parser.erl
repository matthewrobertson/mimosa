-module(mimosa_irc_command_parser).
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").

parse(<<"PING ", Suffix/binary>>) -> 
  {ping, Suffix};
parse(<<"PRIVMSG ", Suffix/binary>>) -> 
  {privmsg, Suffix};
parse(<<":", Suffix/binary>>) -> 
  [_, Rest] = binary:split(Suffix, <<" ">>),
  parse(Rest);
parse(Command) -> {unknown, Command}.


ping_test() ->
  Command = <<"PING :hubbard.freenode.net\r\n">>,
  Result = parse(Command),
  ?_assert(Result =:= {ping, <<":hubbard.freenode.net\r\n">>}).

privmsg_test() ->
  Command = <<":matthew__!textual@S0106602ad072800b.vc.shawcable.net PRIVMSG matthew_abc :whats up\r\n">>,
  Result = parse(Command),
  ?_assert(Result =:= {privmsg, <<"matthew_abc :whats up\r\n">>}).

error_test() ->
  Command = <<"ERROR :Closing Link: S0106602ad072800b.vc.shawcable.net (Ping timeout: 264 seconds)\r\n">>,
  Result = parse(Command).

unknown_test() ->
  Command = <<"Some random stuff\r\n">>,
  Result = parse(Command),
  ?_assert(Result =:= {unknown, Command}).

