%%
%%   Copyright 2012 - 2013 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @description
%%   script server binding
-module(esh_gfsm).
-behaviour(gen_fsm).

-export([
   start/2
  ,start/3
  ,start_link/2
  ,start_link/3

  ,init/1
  ,terminate/3
  ,idle/2
  ,boot/2
  ,loop/2
  ,handle_event/3
  ,handle_sync_event/4
  ,handle_info/3
  ,code_change/4
]).

%%
%% internal state
-record(fsm, {
   port     = undefined :: any(),     %% erlang port
   longlive = true      :: boolean(), %% long live port
	pid      = undefined :: any()      %% owner process
}).

%%
%%
start(Script, Opts) ->
   gen_fsm:start(?MODULE, [Script, Opts], []).
start(Name, Script, Opts) ->
   gen_fsm:start({local, Name}, ?MODULE, [Script, Opts], []).

start_link(Cmd, Opts) ->
   gen_fsm:start_link(?MODULE, [Cmd, Opts], []).
start_link(Name, Cmd, Opts) ->
   gen_fsm:start_link({local, Name}, ?MODULE, [Cmd, Opts], []).

%%%------------------------------------------------------------------
%%%
%%% Factory
%%%
%%%------------------------------------------------------------------   

init([Script, Opts]) ->
   erlang:process_flag(trap_exit, true),
   {ok, idle,
      #fsm{
			port = esh_port:new(Script, Opts)
      }
   }.

terminate(_, _Sid, #fsm{port = Port}) ->
	esh_port:free(esh_port:kill(Port)).

%%%------------------------------------------------------------------
%%%
%%% loop
%%%
%%%------------------------------------------------------------------   

%%
%% idle - script is not running
idle({bind, Pid}, State) ->
	{next_state, idle, State#fsm{pid = Pid}};

idle(run, #fsm{port = Port}=State) ->
   {next_state, boot, 
		State#fsm{
			port = esh_port:spawn(Port)
		}
	};

idle(run_once, #fsm{port = Port}=State) ->
   {next_state, boot, 
		State#fsm{
			port = esh_port:spawn(Port), longlive = false
		}
	};

idle(Msg, #fsm{port = Port}=State)
 when is_binary(Msg) ->
	{next_state, boot, 
		State#fsm{
			port = esh_port:send(Msg, esh_port:spawn(Port))
		}
	};
   
idle(close, State) ->
   {stop, normal, State};

idle({'EXIT', _, _}, State) ->
   %% ignore port exit command
   {next_state, idle, State};

idle(Msg, State) ->
   error_logger:warning_msg("esh [idle]: unknown message ~p~n", [Msg]),
   {next_state, idle, State}.

%%
%% boot - script is booting up 
%% the first required message is pid of unix process
boot({_Port, {data, <<$p, $i, $d, $ , Pid/binary>>}}, #fsm{port = Port}=State) ->
   {next_state, loop, 
		State#fsm{
			port = esh_port:pid(Pid, Port)
		}
	};

boot(close, State) ->
   {stop, normal, State};

boot(Msg, State) ->
   error_logger:warning_msg("esh [boot]: unknown message ~p~n", [Msg]),
   {next_state, boot, State}.


%%
%% loop - script is running i/o
loop({_Port, {exit_status, Code}}, #fsm{longlive=true, pid=Pid}=State) ->
   Pid ! {esh, self(), {eof, Code}},
   {next_state, idle, State};

loop({_Port, {exit_status, Code}}, #fsm{pid=Pid}=State) ->
   Pid ! {esh, self(), {eof, Code}},
   {stop, normal, State};

loop({_Port, {data, Pckt}}, #fsm{pid=Pid}=State) ->
   Pid ! {esh, self(), Pckt},
   {next_state, loop, State};

loop(close, State) ->
   {stop, normal, State};

loop(Msg, #fsm{port = Port}=State)
 when is_binary(Msg) ->
   {next_state, loop, 
		State#fsm{
			port = esh_port:send(Msg, Port)
		}
	};

loop(Msg, State) ->
   error_logger:warning_msg("esh [loop]: unknown message ~p~n", [Msg]),
   {next_state, boot, State}.

%%
%%
handle_event(Msg, Sid, State) ->
   ?MODULE:Sid(Msg, State).

%%
%%
handle_info(Msg, Sid, State) ->
   ?MODULE:Sid(Msg, State).

%%
%%
handle_sync_event(Msg, Tx, Sid, State) ->
   ?MODULE:Sid(Msg, Tx, State).

%%
%%
code_change(_Vsn, Sid, State, _Extra) ->
   {ok, Sid, State}.

