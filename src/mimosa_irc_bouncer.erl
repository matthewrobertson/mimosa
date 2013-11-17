-module(mimosa_irc_bouncer).

-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start/0, stop/1]).

-record(state, {socket}).


start() ->
  lager:info("Starting IRC bouncer"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

send_message(Socket, Msg)->
  gen_tcp:send(Socket, Msg).

init([]) ->
  lager:info("Init IRC test"),
  {ok, Socket} = gen_tcp:connect("irc.freenode.net", 6667, [binary, {active,true}]),
  send_message(Socket, mimosa_irc_message_builder:nick("matthew_abc")),
  send_message(Socket, mimosa_irc_message_builder:user("matthew_abc", "Matt R")),
  {ok, #state{socket=Socket}}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

handle_call(stop, _Caller, State) ->
  Socket = State#state.socket,
  send_message(Socket, mimosa_irc_message_builder:quit()),
  {stop, normal, ok, State};
handle_call(_Req, _Caller, State) ->
  lager:info("Call IRC test"),
  {reply, sweet, State}.

handle_cast(_Req, State) ->
  lager:info("Cast IRC test"),
  {noreply, State}.

handle_info({tcp, _Port, Stuff}, State) ->
  case mimosa_irc_command_parser:parse(Stuff) of
    {ping, Server} ->
      lager:info("got a ping"),
      Socket = State#state.socket,
      send_message(Socket, mimosa_irc_message_builder:pong(Server)),
      {noreply, State};
    {privmsg, Msg} ->
      lager:info("GOT A MESSAGE:"),
      lager:info("~p", [Msg]),
      {noreply, State};
    {_, X} ->
      lager:info("=========== unhandled command ==============="),
      lager:info("~p", [X]),
      {noreply, State}
  end;
handle_info(_Req, State) ->
  lager:info("Info IRC test"),
  {noreply, State}.

terminate(_Reason, _State) ->
  lager:info("Terminate IRC test"),
  ok.

