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
%%   script binding
-module(esh_script).
-behaviour(pipe).

-export([
	start/2,
	start/3,
	start_link/2,
	start_link/3,

	init/1,
	free/2,
	ioctl/2,
	loop/3
]).

%% script bootstrap code
-define(BOOTSTRAP, "
   echo \"pid $$\";
   exec ~s ~s
").

%% line size
-define(IOLINE,  16 * 1024).

%%
%% internal state
-record(fsm, {
	script   = undefined :: list(),
	opts     = undefined :: list(),   %% port opts
	port     = undefined :: any(),    %% shell script port
	pid      = undefined :: list(),   %% unix process
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
   {ok, loop, 
   	#fsm{
   		script = find_script(Script),
   		opts   = Opts 
   	}
   }.

free(_, S) ->
	_ = close_port(S),
	_ = kill_script(S),
	ok.

ioctl(_, _) ->
	throw(not_supported).


%%%------------------------------------------------------------------
%%%
%%% loop
%%%
%%%------------------------------------------------------------------   

%%
%% inbound
loop({_Port, {exit_status, Code}}, Pipe, #fsm{longlive=true}=S) ->
   pipe:a(Pipe, {eof, Code}),
   {next_state, loop, 
      S#fsm{
         port = undefined,
         pid  = undefined
      }
   };

loop({_Port, {exit_status, Code}}, Pipe, S) ->
	pipe:a(Pipe, {eof, Code}),
   {stop, normal, S};

loop({_Port, {data, {eol, <<$p,$i, $d, $ , Pid/binary>>}}}, _Pipe, #fsm{pid=undefined}=S) ->
	{next_state, loop, S#fsm{pid = binary_to_list(Pid)}};

loop({_Port, {data, {noeol, Line}}}, Pipe, S) ->
	pipe:a(Pipe, Line),
   {next_state, loop, S};

loop({_Port, {data, {eol, Line}}}, Pipe, S) ->
	pipe:a(Pipe, <<Line/binary, $\n>>),
   {next_state, loop, S};

%%
%% outbound
loop(close, _Pipe, S) ->
   {stop, normal, S};

loop(Msg, _Pipe, #fsm{port=undefined}=S)
 when is_binary(Msg) ->
   Port = spawn_script(S),
   _ = erlang:port_command(Port, Msg),
   {next_state, loop, 
      S#fsm{
         port = Port,
         pid  = undefined
      }
   };

loop(Msg, _Pipe, #fsm{}=S)
 when is_binary(Msg) ->
   _ = erlang:port_command(S#fsm.port, Msg),
   {next_state, loop, S};

loop(run, _Pipe, #fsm{port=undefined}=S) ->
   {next_state, loop, 
      S#fsm{
         port = spawn_script(S),
         pid  = undefined
      }
   };

loop(run_once, _Pipe, #fsm{port=undefined}=S) ->
   {next_state, loop, 
      S#fsm{
         port = spawn_script(S),
         pid  = undefined,
         longlive = false
      }
   };

loop(_, _Pipe, #fsm{}=S) ->
   {next_state, loop, S}.


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------   

%%
%%
find_script(Script)
 when is_atom(Script) ->
   os:find_executable(Script);

find_script(Script)
 when is_binary(Script) ->
 	binary_to_list(Script);

find_script(Script)
 when is_list(Script) ->
 	Script.

%%
%%
close_port(#fsm{port=undefined}) ->
	ok;
close_port(#fsm{port=Port}) ->
   case erlang:port_info(Port) of
   	undefined ->
   		ok;
   	_ ->
			erlang:port_close(Port)
	end.

%%
%%
kill_script(#fsm{pid=undefined}) ->
	ok;
kill_script(#fsm{pid=Pid}) ->
   os:cmd("pkill -9 -P " ++ Pid),
   os:cmd("kill -9 " ++ Pid).


%%
%%
spawn_script(#fsm{script=Script, opts=Opts}) ->
   Shell = os:find_executable(sh),
   Cmd   = case lists:keyfind(args, 1, Opts) of
      {args, Args} -> io_lib:format(?BOOTSTRAP, [Script, string:join(Args, " ")]);
      _            -> io_lib:format(?BOOTSTRAP, [Script, ""])
   end,
   erlang:open_port(
      {spawn_executable, Shell},
      [
         {args, ["-c", Cmd]},
         {line, ?IOLINE}, binary, stream,
         exit_status, use_stdio, stderr_to_stdout, hide
      ] ++ port_opts(Opts)
   ).

port_opts(Opts) ->
   port_opts(Opts, []).
port_opts([{cd,  _}=X | Opts], Acc) ->
   port_opts(Opts, [X | Acc]);
port_opts([{env, _}=X | Opts], Acc) ->
   port_opts(Opts, [X | Acc]);
port_opts([_ | Opts], Acc) ->
   port_opts(Opts, Acc);
port_opts([], Acc) ->
   Acc.


