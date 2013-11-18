-module(mimosa_irc_command_parser).
-export([parse/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(command, {type, prefix, params}).
-record(command_prefix, {name, user, host}).

parse(Input) -> parse(Input, #command{}).

parse(<<":", Suffix/binary>>, Result) ->
  % the prefix is everything up to the first ' '
  [Prefix, Rest] = binary:split(Suffix, <<" ">>),
  parse(Rest, Result#command{prefix=parse_prefix(Prefix)});
parse(<<"PING", Suffix/binary>>, Result) ->
  Result#command{type=ping, params=parse_params(Suffix)};
parse(<<"PRIVMSG", Suffix/binary>>, Result) ->
  Result#command{type=privmsg, params=parse_params(Suffix)};
parse(<<"ERROR", Suffix/binary>>, Result) ->
  Result#command{type=error, params=parse_params(Suffix)};
parse(<<"NOTICE", Suffix/binary>>, Result) ->
  Result#command{type=error, params=parse_params(Suffix)};
parse(Command, _Result) ->
  {unknown, Command}.

parse_params(<<" ", Params/binary>>) ->
  parse_params(Params);
parse_params(Params) ->
  binary:replace(Params, <<"\r\n">>, <<"">>).

parse_prefix(Prefix) ->
  % TODO this is not very robust
  case binary:split(Prefix, [<<"@">>, <<"!">>], [global, trim]) of
    [Name, User, Host] ->
      #command_prefix{name=Name, user=User, host=Host};
    [Name] ->
      #command_prefix{name=Name};
    _Else ->
      #command_prefix{}
  end.

-ifdef(TEST).

ping_test() ->
  Command = <<"PING :hubbard.freenode.net\r\n">>,
  Result = parse(Command),
  ?assertEqual(ping, Result#command.type),
  ?assertEqual(<<":hubbard.freenode.net">>, Result#command.params),
  ok.

privmsg_test() ->
  Command = <<":matthew__!~textual@S0106586d8f63ed2e.ok.shawcable.net PRIVMSG matthew_abc :yo\r\n">>,
  Result = parse(Command),
  ?assertEqual(privmsg, Result#command.type),
  ?assertEqual(<<"matthew_abc :yo">>, Result#command.params),
  ok.

error_test() ->
  Command = <<"ERROR :Closing Link: S0106602ad072800b.vc.shawcable.net (Ping timeout: 264 seconds)\r\n">>,
  Result = parse(Command),
  ?assertEqual(error, Result#command.type),
  ?assertEqual(<<":Closing Link: S0106602ad072800b.vc.shawcable.net (Ping timeout: 264 seconds)">>, Result#command.params),
  ok.

unknown_test() ->
  Command = <<"Some random stuff\r\n">>,
  ?assertEqual({unknown, Command}, parse(Command)),
  ok.

parse_prefix_test() ->
  Prefix = <<"matthew__!~textual@S0106586d8f63ed2e.ok.shawcable.net">>,
  Result = parse_prefix(Prefix),
  ?assertEqual(<<"matthew__">>, Result#command_prefix.name),
  ?assertEqual(<<"~textual">>, Result#command_prefix.user),
  ?assertEqual(<<"S0106586d8f63ed2e.ok.shawcable.net">>, Result#command_prefix.host),
  ok.

-endif.
