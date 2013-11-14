-module(mimosa_irc_command_parser).
-export([parse/1]).
-include_lib("eunit/include/eunit.hrl").

parse(<<"PING ", Suffix/binary>>) -> {ping, Suffix};
parse(Suffix) -> {unknown, Suffix}.


parse_test_() ->
  [?_assert(parse(<<"PING :hubbard.freenode.net\r\n">>) =:= {ping, <<":hubbard.freenode.net\r\n">>})
  ].
