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
   idle/3,
   boot/3,
   loop/3
]).

%%
%% script bootstrap code
-define(BOOTSTRAP, "echo \"pid $$\"; exec ~s ~s").

-define(PORT(X), [
   {args, ["-c", X]}
  ,binary
  ,stream
  ,exit_status
  ,use_stdio
  ,stderr_to_stdout
  ,hide
]).



%% line size
-define(IOLINE,  16 * 1024).

%%
%% internal state
-record(fsm, {
   script   = undefined :: list(),   %% script to execute
   args     = undefined :: list(),   %% script arguments 
   opts     = undefined :: list(),   %% port opts
   port     = undefined :: any(),    %% erlang port
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
   {ok, idle, 
      #fsm{
         script = find_script(hd(Script)),
         args   = make_args(tl(Script)),
         opts   = port_opts(Opts) 
      }
   }.

free(_, State) ->
   _ = close_port(State),
   _ = kill_script(State),
   ok.

ioctl(_, _) ->
   throw(not_supported).

%%%------------------------------------------------------------------
%%%
%%% loop
%%%
%%%------------------------------------------------------------------   

%%
%% idle - script is not running
idle(run, _Pipe, #fsm{}=S) ->
   {next_state, boot, S#fsm{port = spawn_script(S)}};

idle(run_once, _Pipe, #fsm{}=S) ->
   {next_state, boot, S#fsm{port = spawn_script(S), longlive = false}};

idle(Msg, _Pipe, #fsm{}=S)
 when is_binary(Msg) ->
   Port = spawn_script(S),
   _ = erlang:port_command(Port, Msg),
   {next_state, boot, S#fsm{port = Port}};

idle(close, _Pipe, S) ->
   {stop, normal, S};

idle({'EXIT', _, _}, _Pipe, S) ->
   %% ignore port exit command
   {next_state, idle, S};

idle(Msg, _Pipe, S) ->
   error_logger:warning_msg("esh [idle]: unknown message ~p~n", [Msg]),
   {next_state, idle, S}.

%%
%% boot - script is booting up 
%% the first required message is pid of unix process
boot({_Port, {data, <<$p, $i, $d, $ , Pid/binary>>}}, _Pipe, #fsm{}=S) ->
   {next_state, loop, S#fsm{pid = binary_to_list(Pid)}};

boot(close, _Pipe, S) ->
   {stop, normal, S};

boot(Msg, _Pipe, S) ->
   error_logger:warning_msg("esh [boot]: unknown message ~p~n", [Msg]),
   {next_state, boot, S}.


%%
%% loop - script is running i/o
loop({_Port, {exit_status, Code}}, Pipe, #fsm{longlive=true}=S) ->
   pipe:a(Pipe, {esh, self(), {eof, Code}}),
   {next_state, idle, 
      S#fsm{
         port = undefined,
         pid  = undefined
      }
   };

loop({_Port, {exit_status, Code}}, Pipe, S) ->
   pipe:a(Pipe, {esh, self(), {eof, Code}}),
   {stop, normal, S};

loop({_Port, {data, Pckt}}, Pipe, S) ->
   pipe:a(Pipe, {esh, self(), Pckt}),
   {next_state, loop, S};

loop(close, _Pipe, S) ->
   {stop, normal, S};

loop(Msg, _Pipe, #fsm{}=S)
 when is_binary(Msg) ->
   _ = erlang:port_command(S#fsm.port, Msg),
   {next_state, loop, S};

loop(Msg, _Pipe, S) ->
   error_logger:warning_msg("esh [loop]: unknown message ~p~n", [Msg]),
   {next_state, boot, S}.


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
%% make script arguments
make_args(false) ->
   "";
make_args(undefined) ->
   "";
make_args({args, X}) ->
   X;
make_args(X)
 when is_list(X) ->
   X.

%%
%% make port options
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
spawn_script(#fsm{}=S) ->
   Shell = os:find_executable(sh),
   Cmd   = lists:flatten(
      io_lib:format(?BOOTSTRAP, [S#fsm.script, string:join(S#fsm.args, " ")])
   ),
   erlang:open_port({spawn_executable, Shell}, ?PORT(Cmd) ++ S#fsm.opts).



