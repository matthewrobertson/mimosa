-module(mimosa_irc_command_builder).

-export([
    nick/1,
    pong/1,
    privmsg/2,
    quit/0,
    user/2
  ]).

nick(Nick) ->
  [<<"NICK ">>, Nick, <<"\r\n">>].

pong(Server) ->
  [<<"PONG ">>, Server, <<"r\n">>].

privmsg(To, Msg) ->
  [<<"PRIVMSG ">>, To, <<" :">>, Msg, <<"\r\n">>].

quit() ->
  <<"QUIT \r\n">>.

user(Nick, Name) ->
  [<<"User ">>, Nick, <<" 8 * : ">>, Name, <<"\r\n">>].
