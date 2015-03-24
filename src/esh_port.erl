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
%%   external port helper functions
-module(esh_port).

-export([
	new/2
  ,free/1
  ,spawn/1
  ,kill/1
  ,send/2
  ,pid/2
]).

%% internal state
-record(state, {
	script = undefined :: any()   %% script/command to execute
  ,args   = undefined :: any()   %% command line arguments
  ,opts   = undefined :: list()  %% port options
  ,port   = undefined :: port()  %% port reference
  ,pid    = undefined :: any()   %% native process reference
}).

%% script bootstrap code
-define(BOOTSTRAP, "echo \"pid $$\"; exec ~s ~s").

%% global port options
-define(PORT(X), [
   {args, ["-c", X]}
  ,binary
  ,stream
  ,exit_status
  ,use_stdio
  ,stderr_to_stdout
  ,hide
]).

%%
%% create new port
-spec(new/2 :: (list(), list()) -> #state{}).

new([Script | Args], Opts) ->
	#state{
		script = script(Script)
	  ,args   = string:join([to_list(X) || X <- Args], " ")
	  ,opts   = lists:foldl(fun opts/2, [], Opts)
	}.

%%
%% translate port native options
opts({cd,  _}=X, Opts) -> [X | Opts];
opts({env, _}=X, Opts) -> [X | Opts];
opts(_         , Opts) -> Opts.

%%
%% find path to script
script(Script)
 when is_atom(Script) ->
   os:find_executable(Script);
script(Script)
 when is_binary(Script) ->
   binary_to_list(Script);
script(Script)
 when is_list(Script) ->
   Script.

%%
%%
to_list(X)
 when is_atom(X) ->
	atom_to_list(X);
to_list(X)
 when is_binary(X) ->
	binary_to_list(X);
to_list(X)
 when is_integer(X) ->
	integer_to_list(X);
to_list(X)
 when is_list(X) ->
	X.

%%
%% free port resources
-spec(free/1 :: (#state{}) -> ok).

free(#state{port=undefined}) ->
	ok;
free(#state{port=Port}) ->
	erlang:port_info(Port) =/= undefined andalso erlang:port_close(Port).

%%
%% spawn process associated with port
-spec(spawn/1 :: (#state{}) -> #state{}).

spawn(#state{script = Script, args = Args, opts = Opts}=State) ->
   Shell = os:find_executable(sh),
   Cmd   = lists:flatten(
      io_lib:format(?BOOTSTRAP, [Script, Args])
   ),
	State#state{
		port = erlang:open_port({spawn_executable, Shell}, ?PORT(Cmd) ++ Opts)
	}.

%%
%% kill process associated with port
-spec(kill/1 :: (#state{}) -> #state{}).

kill(#state{pid=undefined}=State) ->
   State;
kill(#state{pid=Pid}=State) ->
   os:cmd("pkill -9 -P " ++ Pid),
   os:cmd("kill -9 " ++ Pid),
	State#state{pid=undefined}.

%%
%% send message to port
-spec(send/2 :: (binary(), #state{}) -> #state{}).

send(Msg, #state{port = Port}=State) ->
   _ = erlang:port_command(Port, Msg),
	State.

%%
%% set pid of native process
-spec(pid/2 :: (list() | binary(), #state{}) -> #state{}).

pid(Pid, State) ->
	State#state{pid=to_list(Pid)}.

