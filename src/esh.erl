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
	run/3,
	run_link/1,
	run_link/2,
	run_link/3
]).

-type(script() :: atom() | list() | binary()).

%%
%% spawn shell scripts port and bind it to current process
-spec(spawn/1 :: (script()) -> {ok, any()} | {error, any()}).
-spec(spawn/2 :: (script(), list()) -> {ok, any()} | {error, any()}).
-spec(spawn/3 :: (atom(), script(), list()) -> {ok, any()} | {error, any()}).
-spec(spawn_link/1 :: (script()) -> {ok, any()} | {error, any()}).
-spec(spawn_link/2 :: (script(), list()) -> {ok, any()} | {error, any()}).
-spec(spawn_link/3 :: (atom(), script(), list()) -> {ok, any()} | {error, any()}).

spawn(Script) ->
	esh:spawn(Script, []).
spawn(Script, Args) ->
	do_spawn(start, Script, Args).
spawn(Name, Script, Args) ->
	do_spawn(start, Name, Script, Args).

spawn_link(Script) ->
	esh:spawn_link(Script, []).
spawn_link(Script, Args) ->
	do_spawn(start_link, Script, Args).
spawn_link(Name, Script, Args) ->
	do_spawn(start_link, Name, Script, Args).

do_spawn(Fun, Script, Args) ->
	do_bind(esh_script:Fun(Script, Args)).

do_spawn(Fun, Name, Script, Args) ->
	do_bind(esh_script:Fun(Name, Script, Args)).

do_bind({ok, Pid}) ->
	pipe:bind(a, Pid, self()),
	pipe:send(Pid, run),
	{ok, Pid};
do_bind(Error) ->
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
%%    {output, function()} - output mapper function (default identity)
%%    return_on_error      - return script output 
-spec(run/1 :: (script()) -> {ok, any()} | {error, any()}).
-spec(run/2 :: (script(), list()) -> {ok, any()} | {error, any()}).
-spec(run/3 :: (script(), list(), list()) -> {ok, any()} | {error, any()}).
-spec(run_link/1 :: (script()) -> {ok, any()} | {error, any()}).
-spec(run_link/2 :: (script(), list()) -> {ok, any()} | {error, any()}).
-spec(run_link/3 :: (script(), list(), list()) -> {ok, any()} | {error, any()}).

run(Script) ->
	run(Script, [], []).
run(Script, Args) ->
	run(Script, Args, []).
run(Script, Args, Opts) ->
	do_run(start, Script, Args, Opts).

run_link(Script) ->
	run_link(Script, [], []).
run_link(Script, Args) ->
	run(Script, Args, []).
run_link(Script, Args, Opts) ->
	do_run(start_link, Script, Args, Opts).

do_run(Fun, Script, Args, Opts) ->
	case esh_script:Fun(Script, Args) of
		{ok, Pid} ->
			pipe:bind(a, Pid, self()),
			pipe:send(Pid, run_once),
			run_loop(
				proplists:get_value(return_on_error, Opts),
				proplists:get_value(timeout, Opts, infinity),
				proplists:get_value(output,  Opts, fun(X) -> X end),
				[]
			);
		Error ->
			Error
	end.

%%
%%
run_loop(Return, Timeout, Fun, Acc) ->
	case pipe:recv(Timeout) of
		{eof, 0}    ->
			{ok, lists:reverse(Acc)};
		{eof, Code} ->
			case Return of
				true -> {error, lists:reverse(Acc)};
				_    -> {error, Code}
			end;
		Msg when is_binary(Msg) ->
			run_loop(Return, Timeout, Fun, [Fun(Msg) | Acc])
	end.


