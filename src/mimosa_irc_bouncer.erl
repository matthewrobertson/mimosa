-module(mimosa_irc_bouncer).

-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start/0, stop/1]).

-record(command, {type, prefix, params}).
-record(state, {socket, data}).


start() ->
  lager:info("Starting IRC bouncer"),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

%% BEGIN:: PUT THIS SHIT IN ITS OWN MODULE %%
send_message(Socket, Msg)->
  gen_tcp:send(Socket, Msg).

run_command(Socket, Stuff) ->
  Command = mimosa_irc_command_parser:parse(Stuff),
  case Command of
    #command{type=ping, params=Server} ->
      lager:info("got a ping"),
      send_message(Socket, mimosa_irc_command_builder:pong(Server));
    #command{type=privmsg, params=Msg, prefix=From} ->
      lager:info("got a message from ~p", [From]),
      lager:info("~p", [Msg]);
    X ->
      lager:info("=========== unhandled command ==============="),
      lager:info("~p", [X])
  end.

handle_commands(Stuff, State) ->
  Socket = State#state.socket,
  Commands = binary:split(Stuff, <<"\r\n">>, [global, trim]),
  Fun = fun(C) -> run_command(Socket, C) end,
  lists:foreach(Fun, Commands).
%% END:: PUT THIS SHIT IN ITS OWN MODULE %%

init([]) ->
  lager:info("Init IRC test"),
  {ok, Socket} = gen_tcp:connect("irc.freenode.net", 6667, [binary, {active,true}]),
  send_message(Socket, mimosa_irc_command_builder:nick("matthew_abc")),
  send_message(Socket, mimosa_irc_command_builder:user("matthew_abc", "Matt R")),
  Data = <<"">>,
  {ok, #state{socket=Socket, data=Data}}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

handle_call(stop, _Caller, State) ->
  Socket = State#state.socket,
  send_message(Socket, mimosa_irc_command_builder:quit()),
  {stop, normal, ok, State};
handle_call(_Req, _Caller, State) ->
  lager:info("Call IRC test"),
  {reply, sweet, State}.

handle_cast(_Req, State) ->
  lager:info("Cast IRC test"),
  {noreply, State}.

handle_info({tcp, _Port, Stuff}, State) ->
  case mimosa_irc_data_framer:frame(State#state.data, Stuff) of
    {incomplete, Newdata} ->
      {noreply, State#state{data=Newdata}};
    {complete, Data} ->
      handle_commands(Data, State),
      Reset = <<"">>,
      {noreply, State#state{data=Reset}}
  end;
handle_info(_Req, State) ->
  lager:info("Info IRC test"),
  {noreply, State}.

terminate(_Reason, _State) ->
  lager:info("Terminate IRC test"),
  ok.

