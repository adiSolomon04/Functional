-module(ex8).
-author("adisolo").

%% API
-export([startChat/1, call/1, steadyLink/1, steadyMon/1]).


%%%------ startChat function -----------

%%----------------------Export Functions--------------------------
%%The client process has:
%% remoteProcess, localProcess, remoteHosts
startChat(RemoteHost) ->
  rpc:call(RemoteHost, ?MODULE, remote_start, []),
  put(remoteHost, RemoteHost),
  register(local, Pid=spawn(fun()-> local_loop(start) end)),
  Pid.

%%call(quit) -> rpc:call(get(remoteHost), ?MODULE, remote_loop,[{whereis(localProcess), quit}]),
%%  io:format("~p - Successfully closed.~n~p - Successfully closed.",[whereis(remoteProcess), whereis(localProcess)]).
%%call(Message) -> rpc:call(get(remoteHost), ?MODULE, remote_loop,[{whereis(localProcess),Message}]).

%% spawns a process in the remote node, sends the message to the
%% process 'remote' that is defined there
call(Message) ->
  rpc:call(get(remoteHost), ?MODULE, remote_call, [Message]).

%%----------------------end Export Functions----------------------

%%----------------------Local-------------------------
local_loop(run) ->
  receive
    stats ->
      io:format("local stats: sent: ~p received: ~p",[get(sent), get(received)]),
      local_loop(run);
    {f, Message} ->
      io:format("~p",[Message]),
      local_loop(run);

  end;
local_loop(start) ->
  put(received, 0),
  put(sent, 0),
  local_loop(run).
local_call(Message) -> local ! Message.
%%---------------------end Local-----------------------


%%----------------------Remote-------------------------

%% register and start the loop of the remote process
%% If don't exist
remote_start() ->
  case whereis(remote) of
    undefined -> register(remote ,self()),
      remote_loop(start);
    _ -> alreadyDefined
  end.


remote_loop(run) ->
  receive

  end
remote_loop(start) ->
  put(received, 0),
  put(sent, 0),
  remote_loop(run).

remote_call(Message) -> remote ! Message.



%%----------------------end Remote--------------------
%%------ end of startChat-----------

steadyLink(F) -> Pid = spawn_link(fun() -> F() end),
  timer:sleep(5000),
  Pid.

steadyMon(F) -> {Pid,Ref} = spawn_monitor(fun() -> F() end),
  receive
    {'DOWN', Ref, process, Pid, normal} ->
      io:format("Normal termination of process ~p was detected",[Pid]);
    {'DOWN', Ref, process, Pid, Why} ->
      io:format("An exception in process ~p was detected: ~p",[Pid,Why])
  after
    5000 -> exit(Pid)
  end,
  Pid.
