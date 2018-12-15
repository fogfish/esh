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
%%   unit test
-module(esh_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([
   test_spawn/1
]).

all() -> 
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      NAry =:= 1
   ].

%%
%%
test_spawn(Config) ->
   Home = ?config(data_dir, Config),
   {ok, Pid} = esh:spawn([sh, "print.sh"], [{cd, Home}]),
   pipe:make([Pid, self()]),
   {ioctl, a, Pid} = pipe:recv(),
   pipe:send(Pid, run),
   {esh, Pid, <<"==> start\n">>} = pipe:recv(),
   {esh, Pid, <<"==> echo 1\n">>} = pipe:recv(),
   {esh, Pid, <<"==> echo 2\n">>} = pipe:recv(),
   {esh, Pid, <<"==> echo 3\n">>} = pipe:recv(),
   {esh, Pid, <<"==> echo 4\n">>} = pipe:recv(),
   {esh, Pid, <<"==> echo 5\n">>} = pipe:recv(),
   pipe:free(Pid).
