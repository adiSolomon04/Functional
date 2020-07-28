-module(ex8).
-author("adisolo").

%% API
-export([startChat/1, call/1, steadyLink/1, steadyMon/1]).

-compile(export_all).
%%%------ startChat function -----------

%%----------------------Export Functions--------------------------
%%The client process has:
%% remoteProcess, localProcess, remoteHosts
startChat(RemoteNode) ->
  rpc:call(RemoteNode, ex8, remote_start, [node()]), %%remote_start, node()
  put(remote_node, RemoteNode),
  register(local, Pid=spawn(fun()-> local_loop(RemoteNode) end)),
  Pid.

%% spawns a process in the remote node, sends the message to the
%% process 'remote' that is defined there
call(Message) ->
  rpc:call(get(remote_node), ?MODULE, remote_call, [Message]).

echo(Pid) ->Pid!hello.

%%call(quit) -> rpc:call(get(remoteHost), ?MODULE, remote_loop,[{whereis(localProcess), quit}]),
%%  io:format("~p - Successfully closed.~n~p - Successfully closed.",[whereis(remoteProcess), whereis(localProcess)]).
%%call(Message) -> rpc:call(get(remoteHost), ?MODULE, remote_loop,[{whereis(localProcess),Message}]).
%%----------------------end Export Functions----------------------

%%----------------------Local-------------------------

%% first, save the remote node and
%% define the variables in dictionary
%% Afterwards - wait for messages from
%% local shell or remote process

local_loop() ->
  receive
    stats ->
      io:format("local stats: sent: ~p received: ~p~n",[get(sent), get(received)]),
      local_loop();
    {print, Message} -> io:format("~p~n", [Message]),
      put(received, get(received)+1),
      local_loop();
    {print_stats, {Sent, Received}} ->
      io:format("remote stats: sent: ~p received: ~p~n",[Sent, Received]),
      local_loop();
    {quit, Pid} -> io:format("~p - Successfully closed.~n", [Pid]),
      io:format("~p - Successfully closed.~n",[self()]);
    Message -> rpc:call(get(remote_node), ?MODULE, remote_call, [{local,Message}]),
      io:format("~p",[Message]),
      put(sent, get(sent)+1),
      local_loop()
  end.

local_loop(RemoteNode) ->
  put(received, 0),
  put(sent, 0),
  put(remote_node, RemoteNode),
  local_loop().

local_call(Message) -> local ! Message.
%%---------------------end Local-----------------------


%%----------------------Remote-------------------------

%% register and start the loop of the remote process
%% If don't exist

remote_start(LocalNode) ->
  case whereis(remote) of
    undefined -> register(remote ,spawn(ex8,remote_loop,[LocalNode]));
      %%remote_loop(LocalNode);
    _ -> alreadyDefined
  end.


remote_loop() ->
  receive
    stats -> Stats = {get(sent),get(received)},%%io_lib:format("remote stats: sent: ~p received: ~p",[]),
      rpc:call(get(local_node), ?MODULE, local_call,  [{print_stats, Stats}]),
      remote_loop();
    quit -> %%Quit = lists:flatten(io_lib:format("~p - Successfully closed.",[self()])),
      rpc:call(get(local_node), ?MODULE, local_call,  [{quit, self()}]);
    {local, _} -> %%io:format("~p",[Message]),
      put(received, get(received)+1),
      remote_loop();
    Message ->
      rpc:call(get(local_node), ?MODULE, local_call,  [{print, Message}]),
      put(sent, get(sent)+1),
      remote_loop()
  end.

remote_loop(LocalNode) ->
  put(received, 0),
  put(sent, 0),
  put(local_node, LocalNode),
  remote_loop().

remote_call(Message) -> remote !Message.%%io_lib:format("~p~n",[Message]).



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
