-module(mimosa_irc_client).

-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([start/0]).

-record(state, {socket}).


start() ->
  lager:info("Starting IRC test"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  lager:info("Starting client"),
  {ok, []}.

code_change(_OldVersion, State, _Extra) -> 
  {ok, State}.

handle_call(Req, _Caller, State) ->
  lager:info("Call IRC test"),
  {reply, sweet, State}.

handle_cast(Req, State) ->
  lager:info("Cast IRC test"),
  {noreply, State}.

handle_info(Req, State) ->
  lager:info("Info IRC test"),
  {noreply, State}.

terminate(_Reason, _State) ->
  lager:info("Terminate IRC test"),
  ok.