-module(ex5).

%% API
-export([]).

ring_parallel(N,M) when is_number(N), is_number(M) andalso N>1, M>1 -> create_n_process(N,M, erlang:timestamp());
ring_parallel(_,_) -> input_error.

create_n_process(N, M, Start_time) -> Last_pid = spawn(fun() -> process_parallel_func(N, N, M, 0, Start_time), 
																		create_n_process(N-1, N, M, Last_pid, Start_Time).

create_n_process(1,N,M,Next_pid), Start_Time -> spawn(fun() -> process_parallel_func(1, N, M, Next_pid, Start_Time) end;
%%create_n_process(N,N,F,_) -> [spawn(fun() -> F end];
create_n_process(I,N,M,Next_pid, Start_Time) -> Id = spawn(fun() -> process_parallel_func(I, N, M, Next_pid, 0) end), 
																							create_n_process(I-1,N,M,Id, Start_Time).

process_parallel_func(Id, N, M, Next_pid, Start_time) -> 
	case Id of 
		1 -> receive
					 Num -> case Num of 
										M -> io:format("total time taken ~f seconds~n", [timer:now_diff(erlang:timestamp(), Start_time) / 1000000]);
										_ -> Next_pid ! {Num+1,self()}, process_parallel_func(Id, N, M, Next_pid, Start_time)
									end
					end
						 
		N -> receive 
					 {Num, First_pid} -> case Num of
															 	M -> First_pid ! Num;
																_ -> First_pid ! Num, process_parallel_func(Id, N, M, 0, 0)
															 end
				 end
			
		_ -> receive 
					 {Num, First_pid} -> case Num of
															 	M -> Next_pid ! {Num, First_pid};
																_ -> Next_pid ! {Num,First_pid}, process_parallel_func(Id, N, M, Next_pid, 0)
															 end
				 end
	end
