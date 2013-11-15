-module(irc_test).

-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start/0, stop/1]).

-record(state, {socket}).


start() ->
  lager:info("Starting IRC test"),
  State = {},
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

init([]) ->
  lager:info("Init IRC test"),
  {ok, Socket} = gen_tcp:connect("irc.freenode.net", 6667, [binary, {active,true}]),
  gen_tcp:send(Socket, "Nick matthew_abc\r\n"),
  gen_tcp:send(Socket, "User matthew_abc 8 * : Matthew Robertson\r\n"),
  {ok, #state{socket=Socket}}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

handle_call(stop, _Caller, State) ->
  gen_tcp:send(State#state.socket, "QUIT :erlang bitches!!!\r\n"),
  {stop, normal, ok, State};
handle_call(Req, _Caller, State) ->
  lager:info("Call IRC test"),
  {reply, sweet, State}.

handle_cast(Req, State) ->
  lager:info("Cast IRC test"),
  {noreply, State}.

handle_info({tcp, _Port, Stuff}, State) ->
  case mimosa_irc_command_parser:parse(Stuff) of
    {ping, Server} ->
      lager:info("got a ping"),
      {noreply, State};
    {_, X} ->
      lager:info("=========== unhandled command ==============="),
      lager:info("~p", [X]),
      {noreply, State}
  end;
handle_info(Req, State) ->
  lager:info("Info IRC test"),
  {noreply, State}.

terminate(_Reason, _State) ->
  lager:info("Terminate IRC test"),
  ok.


  
