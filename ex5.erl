-module(ex5).

%% API
-export([ring_parallel/2, ring_serial/2]).
-export([mesh_serial/3, mesh_parallel/3]).

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
			kill_all_mesh(N),
			{timer:now_diff(Time_process, Start_time), Sent, Messages}
	end;
mesh_parallel(_,_,_) -> input_error.

create_register_process(N) -> lists:foreach(fun(X) -> register(num_to_atom(X), spawn(fun()->mesh_parallel_process2(N)
																																							end))
																		 end,lists:seq(1, N*N)).

create_register_process2(N) -> lists:foreach(fun(X) -> register(num_to_atom(X), spawn(fun()->echo()%%mesh_parallel_process2(N)
																																										 end))
																						end,lists:seq(1, N*N)--[5]),
	register(num_to_atom(5), spawn(fun()->mesh_parallel_process2(N)
																 end)).

kill_all_mesh(N) -> lists:foreach(fun(X)-> case whereis(num_to_atom(X)) of
																						 undefined -> 0; %%only on C
																						 Pid -> exit(Pid, endofProg)
																					 end
																	end, lists:seq(1, N*N)).



mesh_parallel_process2(N) ->
	receive
		{Me,{From,C,_}=Message} when is_number(From)->
			 %% starting message {Me,{all, C, M}} response is {Me, {From, C,M}}
			case get(Message) of
				undefined ->
					%io:format("print 1 ~p~n", [Print]),
					case Me of
						C -> %this is an r message
							case get(m)*(N*N-1)-(get(c)+1) of %%M*(N*N-1) is total number of messages that C need to receive
								Diff when Diff>0 -> put(c, get(c)+1),
									put(Message, 1),
									mesh_parallel_process2(N);
								_ -> %send_die_mesh(N),
									%io:format("print c2 ~p, num~p~n", [get(c)+1, Diff]),
									Pid = get(pid), Pid!{erlang:timestamp(), get(c)+1, get(sent)}
							end;
						%%C when Type==m -> mesh_parallel_process(N,Map#{Message=>1});
						_ -> send_to_neighbors(Me,N,Message),
							put(Message, 1),
							mesh_parallel_process2(N)
					end;
				_ ->mesh_parallel_process2(N)
			end;
		{Me, {all,C,Num}=Message} -> %%receive a starting message - send to neghibors and send response
			%io:format("print 1 ~p~n", [Print]),
			case get(Message) of
				undefined -> send_to_neighbors(Me,N,Response={Me,C,Num}),
					send_to_neighbors(Me,N,Message),
					put(Message, 1),
					put(Response, 1),
					mesh_parallel_process2(N);
				_ -> mesh_parallel_process2(N)
			end;

		{C,M,Pid,start} -> Num_sent = send_messages2(C,N,M), create_map_messages2(C, M),
			put(c, 0), put(pid, Pid), put(m, M), put(sent, Num_sent),
			mesh_parallel_process2(N)%%get_num_neighbors(C,N,1,0)*M*(N-1)})
	end.

create_map_messages2(C ,M) -> lists:map(fun(X) -> put({all,C,X}, 1)
																											end, lists:seq(1, M)).

send_messages2(C,N,M) -> lists:sum(lists:map(fun(Num) -> send_to_neighbors(C, N,{all,C,Num})
																						end, lists:seq(1, M))).

echo() ->
	receive
		Message -> io:format("in process ~p~n", [Message]), echo()
	end.

mesh_parallel_process(N,Map) ->
		receive
			{Me,{To,C,Num}=Message} -> %% starting message {Me,{all, C, M}} response is {Me, {From, C,M}}
				case maps:find(Message,Map) of
					{ok,_} -> mesh_parallel_process(N,Map);
					error ->
						case Me of
							%%To -> send_to_neighbors(Me,N,Response={r,Data}),
							%%	mesh_parallel_process(N,
							%%		Map#{Message=>1,Response=>1});
							C -> %this is an r message
								case maps:get(m,Map)*(N*N-1)-(maps:get(c,Map)+1) of %%M*(N*N-1) is total number of messages that C need to receive
									Num when Num>0 -> MapUpdate = maps:update(c, maps:get(c,Map)+1, Map),
										mesh_parallel_process(N,MapUpdate#{Message=>1});
									_ -> %send_die_mesh(N),
										Pid = maps:get(pid,Map), Pid!{erlang:timestamp(), maps:get(c,Map)+1, maps:get(sent,Map)}
								end;
							%%C when Type==m -> mesh_parallel_process(N,Map#{Message=>1});
							_ -> send_to_neighbors(Me,N,Message),
								case To of %%update maps and send response if needed
									X when is_number(X) ->mesh_parallel_process(N,Map#{Message=>1});
									all -> send_to_neighbors(Me,N,Response={Me,C,Num}),mesh_parallel_process(N,Map#{Message=>1, Response=>1})  %%also send a response
								  %%_ -> nothing %%dont reach it
								end
						end
				end;
			{C,M,Pid,start} -> Num_sent = send_messages(C,N,M), Map =create_map_messages(C, M),
				mesh_parallel_process(N,Map#{c=>0, pid=>Pid, m=>M, sent=>Num_sent})%%get_num_neighbors(C,N,1,0)*M*(N-1)})
		end.

send_messages(C,N,M) -> lists:sum(lists:map(fun(Num) -> send_to_neighbors(C, N,{all,C,Num})
																						end, lists:seq(1, M))).

create_map_messages(C ,M) -> maps:from_list(lists:map(fun(X) -> {{m,{all,C,X}}, 1}
																			 end, lists:seq(1, M))).


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

%send_die_mesh(N) -> lists:foreach(fun(X)-> whereis(num_to_atom(X))!die end, lists:seq(1,N*N)).


%%----------------End MESH parallel

num_to_atom(X) -> list_to_atom(lists:flatten(io_lib:format("~p", [X]))).


%%-----------------MESH Serial----------------------------
mesh_serial(N, M, C) when is_number(N), is_number(M), is_number(C) andalso  M>0, N>1, C=<N*N ->
	Start_time = erlang:timestamp(),
	Pid = spawn(fun() -> mesh_serial_process(N) end),
	%%registered(),
	Pid!{C,M,self(),start},
	receive
		{Time_process, Messages, Sent} ->
			io:format("total time taken ~f seconds~n", [timer:now_diff(Time_process, Start_time) / 1000000]),
			{timer:now_diff(Time_process, Start_time), Sent, Messages}
	end;
mesh_serial(_,_,_) -> input_error.

mesh_serial_process(N) -> %% starting message {Me,{all, C, M}} response is {Me, {From, C,M}}
	receive
		{Me,{From,C,_}=Message}=Print when is_number(From) ->
			%io:format("~p1~n ",[Print]),
			MeMap = get(Me), %%diff
			case maps:find(Message, MeMap) of %%diff
				{ok,_} -> mesh_serial_process(N);
				error ->
					case Me of
						%%To -> send_to_neighbors_serial(Me,N,Response={r,Data}),
						%	NewMeMap= MeMap#{Message=>1,Response=>1},
						%	mesh_serial_process(N,Map#{Me=>NewMeMap}); %%diff
						C ->
							case get(m)*(N*N-1)-(get(c)+1) of %%M*(N*N-1) is total number of messages that C need to receive
								Diff when Diff>0 -> put(c, get(c)+1), NewMeMap = MeMap#{Message=>1},%%diff
									put(Me, NewMeMap),
									mesh_serial_process(N); %%diff
								_ -> Pid = get(pid), Pid!{erlang:timestamp(), get(c)+1, get(sent)} %%die
							end;
						%%C when Type==m -> NewMeMap = MeMap#{Message=>1}, mesh_serial_process(N,Map#{Me=>NewMeMap});
						_ -> send_to_neighbors_serial(Me,N,Message),
							NewMeMap = MeMap#{Message=>1},
							put(Me, NewMeMap),
							mesh_serial_process(N)
					end
			end;
		{Me, {all,C,Num}=Message}=Print ->
			%io:format("~p2~n ",[Print]),
			MeMap = get(Me), %%diff
			case maps:find(Message, MeMap) of %%diff
				{ok,_} -> mesh_serial_process(N);
				error -> send_to_neighbors_serial(Me,N,Response={Me,C,Num}),
					send_to_neighbors_serial(Me,N,Message),
					NewMeMap = MeMap#{Message=>1, Response =>1},
					put(Me, NewMeMap),
					%io:format("~pall1~n ",[Print]),
					mesh_serial_process(N)
			end;
		{C,M,Pid,start} ->
			Num_neighbors = send_messages_serial(C,N,M), create_map_process(N), NewMap=create_map_messages_serial(C,M),
			put(c, 0), put(pid, Pid), put(m, M), put(sent, Num_neighbors*M), put(C, NewMap),
			mesh_serial_process(N)
	end.


create_map_messages_serial(C ,M) -> maps:from_list(lists:map(fun(X) -> {{all,C,X}, 1}
																				end, lists:seq(1, M))).

%% Create a map where each 'node' has a new map
create_map_process(N)-> lists:foreach(fun(X)-> put(X,#{}) end,lists:seq(1, N*N)).

send_messages_serial(C,N,M) -> lists:sum(lists:map(fun(Num) -> send_to_neighbors_serial(C, N,{all,C,Num})%%diff
																																								 end, lists:seq(1, M))).

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

