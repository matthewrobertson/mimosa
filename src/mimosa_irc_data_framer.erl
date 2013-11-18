-module(mimosa_irc_data_framer).
-export([frame/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


frame(A, B) ->
  Bin = <<A/bitstring, B/bitstring>>,
  case binary:part(Bin,{byte_size(Bin), -2}) of
    <<"\r\n">> ->
      {complete, Bin};
    _Else ->
      {incomplete, Bin}
  end.

-ifdef(TEST).

concatenation_test() ->
  {Comp, Result} = frame(<<"old">>, <<"new">>),
  ?assertEqual(<<"oldnew">>, Result),
  ok.

incomplete_framing_test() ->
  {Comp, Result} = frame(<<"old">>, <<"new">>),
  ?assertEqual(incomplete, Comp),
  ok.

complete_framing_test() ->
  {Comp, Result} = frame(<<"old">>, <<"new\r\n">>),
  ?assertEqual(complete, Comp),
  ok.

-endif.
