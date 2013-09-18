# Erlang to Shell Script bindings

The library is a simple wrapper over Erlang port interface. The communication with shell 
script is built on pipe protocol. The library wraps port to process that perform port
supervision and proxies pipe protocol. The major library objective is reliability and fault 
tolerance of shell scripts execution.

The library provides
 * run / run_link      - executes shell script and receive its output
 * spawn / spawn_link  - instantiate pipe object to shell script

## Usage

### Execute shell script once

```bash
   #!/bin/sh
   echo "Hello World."
```

```erlang
   {ok, _} = esh:run("sh helloworld.sh").
```

### Execute shell periodically

```bash
   #!/bin/sh

   read LINE
   echo "$LINE"
``` 

```erlang
	{ok, Pid} = esh:run("sh echo.sh").

   ...

	_ = pipe:send(Pid, <<"Hello\n">>).
	<<"Hello\n">> = pipe:recv().
	{eof, 0} = pipe:recv().

   ...

	_ = pipe:send(Pid, <<"World\n">>).
	<<"World\n">> = pipe:recv().
	{eof, 0} = pipe:recv().

```