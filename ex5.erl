-module(ex5).

%% API
-export([ring_parallel/2, ring_serial/2, mesh_parallel/3]).
-compile(export_all).

%%-------------------------RING-------------------------------
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



%% --------------------MESH---------------------------
%% --------------------MESH Parallel---------------------------

mesh_parallel(N, M, C) when is_number(N), is_number(M), is_number(C) andalso  M>0, N>1, C=<N*N ->
	Start_time = erlang:timestamp(),
	create_register_process(N),
	%%registered(),
	Pid_c = whereis(num_to_atom(C)),
	Pid_c!{C,M,self(),start},
	receive
		{Time_process, Messages, Sent} ->
			io:format("total time taken ~f seconds~n", [timer:now_diff(Time_process, Start_time) / 1000000]),
			{timer:now_diff(Time_process, Start_time), Sent, Messages}
	end;
mesh_parallel(_,_,_) -> input_error.

create_register_process(N) -> lists:foreach(fun(X) -> register(num_to_atom(X), spawn(fun()->mesh_parallel_process(N,#{})%%printer_process(Pid)mesh_parallel_process(N,#{})
																																							end))
																		 end,lists:seq(1, N*N)).
	%unregister('5'),register('5',spawn(fun()->mesh_parallel_process(N,#{}) end)),
	%unregister('2'),register('2',spawn(fun()->mesh_parallel_process(N,#{}) end)).

mesh_parallel_process(N,Map) ->
	receive
		die -> dead
	after
		0 ->
			receive
				{Me,{Type,{To,C,_}=Data}=Message}=Print -> %%io:format("~p print~n",[Print]),
					case maps:find(Message,Map) of
						{ok,_} -> mesh_parallel_process(N,Map);
						error -> io:format("~p print~n",[Print]),
							case Me of
								To -> send_to_neighbors(Me,N,Response={r,Data}),
									mesh_parallel_process(N,
										Map#{Message=>1,Response=>1});
								C when Type==r ->
									case maps:get(m,Map)*(N*N-1)-(maps:get(c,Map)+1) of %%M*(N*N-1) is total number of messages that C need to receive
										Num when Num>0 -> MapUpdate = maps:update(c, maps:get(c,Map)+1, Map),%%Pid = maps:get(pid,Map), Pid!maps:get(c,Map),
											mesh_parallel_process(N,MapUpdate#{Message=>1});
										_ -> send_die_mesh(N), Pid = maps:get(pid,Map), Pid!{erlang:timestamp(), maps:get(c,Map)+1, maps:get(sent,Map)}
									end;
								C when Type==m -> mesh_parallel_process(N,Map#{Message=>1});
								_ -> send_to_neighbors(Me,N,Message),
									mesh_parallel_process(N,Map#{Message=>1})
							end
					end;
				{C,M,Pid,start} -> Num_neighbors = send_messages(C,N,M),
					mesh_parallel_process(N,#{c=>0, pid=>Pid, m=>M, sent=>Num_neighbors*(N*N-1)})%%get_num_neighbors(C,N,1,0)*M*(N-1)})
			after
				0 -> mesh_parallel_process(N,Map)
			end
	end.

send_messages(C,N,M) -> get_num_neighbors(lists:map(fun(X) ->lists:sum(lists:map(fun(Num) -> send_to_neighbors(C, N,{m,{X,C,Num}})
																														 end, lists:seq(1, M)))
																		end,(lists:seq(1, N*N))--[C])).

%% sending messages functions.
send_to_neighbors(Me, N, Message) ->
	send_up(Me, N, Message)+send_down(Me, N, Message)+send_right(Me, N, Message)+send_left(Me, N, Message).
%%send_to_neighbors(_,_,_) -> dont_send.

send_up(Me, N, Message) when Me>N -> SendTo = Me-N, case whereis(num_to_atom(SendTo)) of
																											undefined -> 0;
																											Pid -> Pid!{SendTo,Message}, 1
																										end;
send_up(_,_,_) -> 0.

send_down(Me, N, Message) when Me<N*(N-1) -> SendTo = Me+N, case whereis(num_to_atom(SendTo)) of
																															undefined -> 0;
																															Pid -> Pid!{SendTo,Message}, 1
																														end;
send_down(_,_,_) -> 0.

send_right(Me, N, Message) when Me rem N > 0 -> SendTo = Me+1, case whereis(num_to_atom(SendTo)) of
																																 undefined -> 0;
																																 Pid -> Pid!{SendTo,Message}, 1
																															 end;
send_right(_,_,_) -> 0.

send_left(Me, N, Message) when Me rem N>1;Me rem N==0 -> SendTo = Me-1, case whereis(num_to_atom(SendTo)) of
																																					undefined -> 0;
																																					Pid -> Pid!{SendTo,Message}, 1
																																				end;
send_left(_,_,_) -> 0.


get_num_neighbors([Head|_]) -> Head.
%% Same as send_to_neighbors - counts the number of neighbors

send_die_mesh(N) -> lists:foreach(fun(X)-> whereis(num_to_atom(X))!die end, lists:seq(1,N*N)).

%%----------------End MESH parallel

num_to_atom(X) -> list_to_atom(lists:flatten(io_lib:format("~p", [X]))).


%%-----------------MESH Serial----------------------------
mesh_serial(N, M, C) when is_number(N), is_number(M), is_number(C) andalso  M>0, N>1, C=<N*N ->
	Start_time = erlang:timestamp(),
	Pid =spawn(fun() -> mesh_serial_process(N,#{})end),
	%%registered(),
	Pid!{C,M,self(),start},
	receive
		{Time_process, Messages, Sent} ->
			io:format("total time taken ~f seconds~n", [timer:now_diff(Time_process, Start_time) / 1000000]),
			{timer:now_diff(Time_process, Start_time), Sent, Messages}
	end;
mesh_serial(_,_,_) -> input_error.

mesh_serial_process(N,Map) ->
	receive
		die -> dead
	after
		0 ->
			receive
				{Me,{Type,{To,C,_}=Data}=Message}=Print -> io:format("~p print 1~n",[Print]),
					{ok, MeMap} = maps:find(Me,Map), %%diff
					case maps:find(Message, MeMap) of %%diff
						{ok,_} -> mesh_serial_process(N,Map);
						error -> io:format("~p print 2~n",[Print]),
							case Me of
								To -> send_to_neighbors_serial(Me,N,Response={r,Data}),
									NewMeMap= MeMap#{Message=>1,Response=>1},
									mesh_serial_process(N,Map#{Me=>NewMeMap}); %%diff
								C when Type==r ->
									case maps:get(m,Map)*(N*N-1)-(maps:get(c,Map)+1) of %%M*(N*N-1) is total number of messages that C need to receive
										Num when Num>0 -> MapUpdate = maps:update(c, maps:get(c,Map)+1, Map), NewMeMap = MeMap#{Message=>1},%%diff
											io:format("~p print 3~n",[Print]),
											mesh_serial_process(N,MapUpdate#{Me => NewMeMap}); %%diff
										_ -> Pid = maps:get(pid,Map), Pid!{erlang:timestamp(), maps:get(c,Map)+1, maps:get(sent,Map)}, self()!die
									end;
								C when Type==m -> NewMeMap = MeMap#{Message=>1}, mesh_serial_process(N,Map#{Me=>NewMeMap});
								_ -> send_to_neighbors(Me,N,Message),
									mesh_serial_process(N,Map#{Message=>1})
							end
					end;
				{C,M,Pid,start}=Print -> Num_neighbors = send_messages_serial(C,N,M), NewMap=create_map_process(N),io:format("~p print in c~n",[Print]),
					mesh_serial_process(N,NewMap#{c=>0, pid=>Pid, m=>M, sent=>Num_neighbors*(N*N-1)})
			after
				0 -> mesh_serial_process(N,Map)
			end
	end.

%% Create a map where each 'node' has a new map
create_map_process(N)-> ListMap = lists:map(fun(_)-> #{}
																						end, lists:seq(1, N*N)),
	maps:from_list(lists:zip(lists:seq(1, N*N), ListMap)).

send_messages_serial(C,N,M) -> get_num_neighbors(lists:map(fun(X) ->lists:sum(lists:map(fun(Num) -> send_to_neighbors_serial(C, N,{m,{X,C,Num}})%%diff
																																								 end, lists:seq(1, M)))
																										end,(lists:seq(1, N*N))--[C])).

%% sending messages functions.
send_to_neighbors_serial(Me, N, Message) ->
	send_up_serial(Me, N, Message)+send_down_serial(Me, N, Message)+send_right_serial(Me, N, Message)+send_left_serial(Me, N, Message).
%%send_to_neighbors(_,_,_) -> dont_send.

send_up_serial(Me, N, Message) when Me>N -> SendTo = Me-N, self()!{SendTo,Message}, 1; %%diff in all
send_up_serial(_,_,_) -> 0.

send_down_serial(Me, N, Message) when Me<N*(N-1) -> SendTo = Me+N, self()!{SendTo,Message}, 1;
send_down_serial(_,_,_) -> 0.

send_right_serial(Me, N, Message) when Me rem N > 0 -> SendTo = Me+1, self()!{SendTo,Message}, 1;
send_right_serial(_,_,_) -> 0.

send_left_serial(Me, N, Message) when Me rem N>1;Me rem N==0 -> SendTo = Me-1, self()!{SendTo,Message}, 1;
send_left_serial(_,_,_) -> 0.


%%-----------------------------------
printer_process(Pid)->receive
										 Message ->
											 Pid!Message,printer_process(Pid)
									 end.