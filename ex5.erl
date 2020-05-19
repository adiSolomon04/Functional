-module(ex5).

%% API
-export([ring_parallel/2, ring_serial/2]).

%% creates processes and send the start message to pid1
ring_parallel(N,M) when is_number(N), is_number(M) andalso N>1, M>0 ->
	Start_time = erlang:timestamp(),
	[Pid1| Pids] = create_process_parallel(N,M),
	Pid1! {{[],[first,self()|Pids]++[Pid1]}, 1},
	receive
		{Time_process, Messages} ->
			io:format("total time taken ~f seconds~n", [timer:now_diff(Time_process, Start_time) / 1000000]),
			{timer:now_diff(Time_process, Start_time), Messages, Messages}
	end;
ring_parallel(_,_) -> input_error.

%% same as ring_parallel, difference is that all of the processes are the same.
ring_serial(V,M) when is_number(V), is_number(M) andalso V>1, M>0 ->
	Start_time = erlang:timestamp(),
	[Pid1| Pids] = create_process_serial(V,M),
	Pid1! {{[],[first,self()|Pids]++[Pid1]}, 1},
receive
{Time_process, Messages} ->
io:format("total time taken ~f seconds~n", [timer:now_diff(Time_process, Start_time) / 1000000]),
{timer:now_diff(Time_process, Start_time), Messages, Messages}
end;
ring_serial(_,_) -> input_error.


%% returns N pids of different processes
create_process_parallel(N, M) -> lists:map(fun(_) -> spawn(fun() -> process_func(M)
																													 end)
																					 end, lists:seq(1, N)).

%% returns a list of V equal pids
create_process_serial(V,M) -> Pid1 = spawn(fun() -> process_func(M)end),
	lists:map(fun(_) -> Pid1 end, lists:seq(1, V)).


%% The process function.
%% Each process sends message to the next process in the list.
%% The first process configures the message, and send back data to the main process.
process_func(M) ->
	receive
		{die} -> dead;
		{{List,[Head|Tail]}, Message} -> 	case is_pid(Head) of
																				true -> Head!{{List, Tail}, Message}, process_func(M);
																				false -> [_,Pid2|Pids]=Tail, Pid2!{{Tail,Pids}, Message},
																					process_func(M) %% first message to vertex 1.
																			end;
		{{List,[]}, M} -> [PidMain|Pids]=List, PidMain!{erlang:timestamp(),M},
			Set = sets:from_list(Pids), send_die(sets:to_list(Set)); %%This line removes all duplicates.
		{{List,[]}, Message} ->
			[_,Pid2|Pids]=List, Pid2!{{List,Pids}, Message+1}, process_func(M)
	end.

send_die([Pid|Pids]) -> Pid!{die}, send_die(Pids--[Pid]);
send_die([]) -> done.


mesh_parallel(N, M, C) when is_number(N), is_number(M), is_number(C) andalso  M>0, N>1, C=<N*N ->
	Start_time = erlang:timestamp(),
	[Pid1| Pids] = create_process_serial(V,M),
	Pid1! {{[],[first,self()|Pids]++[Pid1]}, 1},
	receive
		{Time_process, Messages} ->
			io:format("total time taken ~f seconds~n", [timer:now_diff(Time_process, Start_time) / 1000000]),
			{timer:now_diff(Time_process, Start_time), Messages, Messages}
	end;
mesh_parallel(_,_,_) -> input_error.

register_process(N) -> lists:foreach(fun(X) ->register(X, spawn(?MODULE, mesh_parallel_process,[N,#{}]) end,lists:seq(1, N*N)).

mesh_parallel_process(N,Map) -> receive
																	{Me,{Type,{To,C,MessageNum}=Data}=Message} -> case find(Message,Map) of
																																				 			{ok,_} -> mesh_parallel_process(Map);
																																				 			error -> case Me of
																																												To -> send_to_neighbors(Me,N, {r,Data}=Response),
																																													mesh_parallel_process(
																																														Map#{Message=>1,Response=>1});
																																												C when Type==r -> 
																																													mesh_parallel_process(N,maps:update(c, mps:get(c,M), M);
																																												_ -> send_to_neighbors(Me,N, Message),
																																													 mesh_parallel_process(Map#{Message=>1})
																																											end								
																																							end; 
																	{C,M, start} -> receive_messages(C,N,M), mesh_parallel_process(N,#{c=>0})
																	end.

send_messages(C,N,M) -> lists:foreach(fun(X) ->lists:foreach(fun(Num) -> send_to_neighbors(C, N, {m,{X,Num}}) 
																														 end, lists:seq(1, M))
																		end,lists:seq(1, N*N)).
