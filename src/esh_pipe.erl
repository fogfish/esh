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
%%   script pipe binding
-module(esh_pipe).
-behaviour(pipe).

-export([
   start/2,
   start/3,
   start_link/2,
   start_link/3,

   init/1,
   free/2,
   ioctl/2,
   idle/3,
   boot/3,
   loop/3
]).

%%
%% internal state
-record(fsm, {
   port     = undefined :: any(),    %% erlang port
   longlive = true      :: boolean() %% long live port
}).

%%
%%
start(Script, Opts) ->
   pipe:start(?MODULE, [Script, Opts], []).
start(Name, Script, Opts) ->
   pipe:start({local, Name}, ?MODULE, [Script, Opts], []).

start_link(Cmd, Opts) ->
   pipe:start_link(?MODULE, [Cmd, Opts], []).
start_link(Name, Cmd, Opts) ->
   pipe:start_link({local, Name}, ?MODULE, [Cmd, Opts], []).

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

free(_, #fsm{port = Port}) ->
	esh_port:free(esh_port:kill(Port)).

ioctl(_, _) ->
   throw(not_supported).

%%%------------------------------------------------------------------
%%%
%%% loop
%%%
%%%------------------------------------------------------------------   

%%
%% idle - script is not running
idle(run, _Pipe, #fsm{port = Port}=State) ->
   {next_state, boot, 
		State#fsm{
			port = esh_port:spawn(Port)
		}
	};

idle(run_once, _Pipe, #fsm{port = Port}=State) ->
   {next_state, boot, 
		State#fsm{
			port = esh_port:spawn(Port), longlive = false
		}
	};

idle(Msg, _Pipe, #fsm{port = Port}=State)
 when is_binary(Msg) ->
	{next_state, boot, 
		State#fsm{
			port = esh_port:send(Msg, esh_port:spawn(Port))
		}
	};
   
idle(close, _Pipe, State) ->
   {stop, normal, State};

idle({'EXIT', _, _}, _Pipe, State) ->
   %% ignore port exit command
   {next_state, idle, State};

idle(Msg, _Pipe, State) ->
   error_logger:warning_msg("esh [idle]: unknown message ~p~n", [Msg]),
   {next_state, idle, State}.

%%
%% boot - script is booting up 
%% the first required message is pid of unix process
boot({_Port, {data, <<$p, $i, $d, $ , Pid/binary>>}}, _Pipe, #fsm{port = Port}=State) ->
   {next_state, loop, 
		State#fsm{
			port = esh_port:pid(Pid, Port)
		}
	};

boot(close, _Pipe, State) ->
   {stop, normal, State};

boot(Msg, _Pipe, S) ->
   error_logger:warning_msg("esh [boot]: unknown message ~p~n", [Msg]),
   {next_state, boot, S}.


%%
%% loop - script is running i/o
loop({_Port, {exit_status, Code}}, Pipe, #fsm{longlive=true}=State) ->
   pipe:b(Pipe, {esh, self(), {eof, Code}}),
   {next_state, idle, State};

loop({_Port, {exit_status, Code}}, Pipe, State) ->
   pipe:b(Pipe, {esh, self(), {eof, Code}}),
   {stop, normal, State};

loop({_Port, {data, Pckt}}, Pipe, State) ->
   pipe:b(Pipe, {esh, self(), Pckt}),
   {next_state, loop, State};

loop(close, _Pipe, State) ->
   {stop, normal, State};

loop(Msg, _Pipe, #fsm{port = Port}=State)
 when is_binary(Msg) ->
   {next_state, loop, 
		State#fsm{
			port = esh_port:send(Msg, Port)
		}
	};

loop(Msg, _Pipe, State) ->
   error_logger:warning_msg("esh [loop]: unknown message ~p~n", [Msg]),
   {next_state, boot, State}.



