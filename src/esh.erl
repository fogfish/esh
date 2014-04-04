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
%%   Erlang to Shell-script bindings
-module(esh).

-export([
	spawn/1,
	spawn/2,
	spawn/3,
	spawn_link/1,
	spawn_link/2,
	spawn_link/3,
	close/1,

	run/1,
	run/2,
	run_link/1,
	run_link/2
]).

-type(script() :: atom() | list() | binary()).

%%
%% spawn shell scripts port and bind it to current process
%% Options
%%    nobind - no auto binding, the process is not bound to process
%%    norun  - do not execute process 
-spec(spawn/1 :: (script()) -> {ok, any()} | {error, any()}).
-spec(spawn/2 :: (script(), list()) -> {ok, any()} | {error, any()}).
-spec(spawn/3 :: (atom(), script(), list()) -> {ok, any()} | {error, any()}).
-spec(spawn_link/1 :: (script()) -> {ok, any()} | {error, any()}).
-spec(spawn_link/2 :: (script(), list()) -> {ok, any()} | {error, any()}).
-spec(spawn_link/3 :: (atom(), script(), list()) -> {ok, any()} | {error, any()}).

spawn(Script) ->
	esh:spawn(Script, []).
spawn(Script, Opts) ->
	do_spawn(start, Script, Opts).
spawn(Name, Script, Opts) ->
	do_spawn(start, Name, Script, Opts).

spawn_link(Script) ->
	esh:spawn_link(Script, []).
spawn_link(Script, Opts) ->
	do_spawn(start_link, Script, Opts).
spawn_link(Name, Script, Opts) ->
	do_spawn(start_link, Name, Script, Opts).

do_spawn(Fun, Script, Opts) ->
   case proplists:get_value(nobind, Opts) of
      true ->
         esh_script:Fun(Script, Opts);
      _    ->
         do_bind(esh_script:Fun(Script, Opts), Opts)
   end.

do_spawn(Fun, Name, Script, Opts) ->
   case proplists:get_value(nobind, Opts) of
      true ->
         esh_script:Fun(Name, Script, Opts);
      _    ->
      	do_bind(esh_script:Fun(Name, Script, Opts), Opts)
   end.

do_bind({ok, Pid}, Opts) ->
	pipe:bind(a, Pid, self()),
   case proplists:get_value(norun, Opts) of
      true ->
         ok;
      _    ->
	     pipe:send(Pid, run)
   end,
	{ok, Pid};
do_bind(Error, _Opts) ->
	Error.

%%
%% close shell script port
-spec(close/1 :: (pid()) -> ok).

close(Pid) ->
	pipe:send(Pid, close), 
	ok.


%%
%% run shell script
%% Options
%%    {timeout, integer()} - time to wait for script i/o (default infinity)
%%    {output,  function()}- output mapper function (default identity)
%%    silent               - return status code only
%%    verbose              - return script output 
-spec(run/1 :: (script()) -> {ok, any()} | {error, any()}).
-spec(run/2 :: (script(), list()) -> {ok, any()} | {error, any()}).
-spec(run_link/1 :: (script()) -> {ok, any()} | {error, any()}).
-spec(run_link/2 :: (script(), list()) -> {ok, any()} | {error, any()}).

run(Script) ->
	run(Script, []).
run(Script, Opts) ->
	do_run(start, Script, Opts).

run_link(Script) ->
	run_link(Script, []).
run_link(Script, Opts) ->
	do_run(start_link, Script, Opts).

do_run(Fun, Script, Opts) ->
	case esh_script:Fun(Script, Opts) of
		{ok, Pid} ->
			pipe:bind(a, Pid, self()),
			pipe:send(Pid, run_once),
			run_loop(
				Opts,
				proplists:get_value(timeout, Opts, infinity),
				proplists:get_value(output,  Opts, fun(X) -> X end),
				[]
			);
		Error ->
			Error
	end.

%%
%%
run_loop(Opts, Timeout, Fun, Acc) ->
	case pipe:recv(Timeout) of
		{esh, _, {eof, 0}}    ->
			case proplists:get_value(silent, Opts) of
				true -> ok;
				_    -> {ok, lists:reverse(Acc)}
			end;
		{esh, _, {eof, Code}} ->
			case proplists:get_value(verbose, Opts) of
				true -> {error, lists:reverse(Acc)};
				_    -> {error, Code}
			end;
		{esh, _, Msg} when is_binary(Msg) ->
			run_loop(Opts, Timeout, Fun, [Fun(Msg) | Acc])
	end.
