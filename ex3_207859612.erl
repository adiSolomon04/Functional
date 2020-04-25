-module(ex3_207859612).

%% API
-export([sortResLC/1, sortResPM/1, sortResLM/1, mSort/1, qSort/1, filter_g/2, filter_p/2, even/1, fiboR/1, fiboT/1, matElemMult/2]).

%%Built 3 Lists using the list comprehension, sorted and summed them together.
sortResLC(List) -> lists:sort([Head|| Head <-List, Head rem 3 == 0]) ++ lists:sort([Head|| Head <-List, Head rem 3 == 1]) ++ lists:sort([Head|| Head <-List, Head rem 3 == 2]).

  %sortResLC(List,[],[],[])head <-List].

%%Built 3 Lists using the case pattern matching, then sorted, and summed them together.
sortResPM(List) -> sortResPM(List,[],[],[]).
sortResPM([Head|Tail], List0, List1, List2) -> case Head rem 3 of
                                                 0 -> sortResPM(Tail, [Head|List0], List1, List2);
                                                 1 -> sortResPM(Tail, List0, [Head|List1], List2);
                                                 2 -> sortResPM(Tail, List0, List1, [Head|List2])
                                               end;
sortResPM([], List0, List1, List2) -> lists:sort(List0) ++ lists:sort(List1) ++ lists:sort(List2).

%%Sorting twice using the sort function in lists module.
%%First sort is by size of the number.
%%Second size is by the residue of the number.
sortResLM(List) -> lists:sort(fun(A,B) -> A rem 3 <B rem 3 end, lists:sort(fun(A,B) -> A > B end, List)).


%%-------------------------------Start of mSort---------------------------
%%-------------------------------------------
%%-------------------------------------------

%% mSort - Using mergesort, mergeLists, movetoList1, movetoList0, movetoLists FUNCTIONS.
%% Call mergesort to start the sorting.
%% First, put each number in a list - each list will be a part of the merge sort    movetoLists
%% Second, split the list of Lists into 2 lists                                     movetoList1, movetoList0
%% Now I will merge pairs of lists, from the Lists that i split                     mergeLists
%% This is done until there is one List left - which is the result.
mSort(List) -> mergesort(movetoList1(movetoLists(List)), movetoList0(movetoLists(List)), []).

%% Main sorting of mSort. merges the lists and splits the result list
%% to start merging again.
%% Finishes when there is only one list left.
mergesort([Head1|Tail1], [Head2|Tail2], List) -> mergesort(Tail1, Tail2, [mergeLists(Head1, Head2)|List]);
mergesort([], [Head2| []], [Head| List]) -> mergesort([],[], [mergeLists(Head2, Head)|List]);
mergesort([], [], [Head|[]]) -> Head;
mergesort([], [], List) ->mergesort(movetoList1(List), movetoList0(List), []).
%%-------------------------------------------

%% put all numbers in A list - make list of lists.
movetoLists(List) -> [[X]||X<-List].
%%-------------------------------------------

%% merge The pair of lists that was chosen.
mergeLists(List1, List2) -> mergeLists(List1, List2, []).

mergeLists([Head1|Tail1], [Head2|Tail2], List) -> case Head1 < Head2 of
                                                    true -> mergeLists(Tail1, [Head2|Tail2], List ++ [Head1]);
                                                    false -> mergeLists([Head1|Tail1], Tail2, List ++ [Head2])
                                                  end;
mergeLists(List1, [], List) -> List++List1;
mergeLists([], List2, List) -> List++List2.
%%-------------------------------------------

%%Those two functions are used for splitting the list of lists into 2 Lists.
%% In this way either movetoList1 has more members
%% or they both have the same number of members.

%% movetoList1 - takes all the odd position numbers.
movetoList1(List) -> movetoList1(List, [], 0).
movetoList1([Head|Tail], Final, K) -> case K rem 2 of
                                        1 -> movetoList1(Tail, [Head|Final], K+1);
                                        0 -> movetoList1(Tail, Final, K+1)
                                      end;
movetoList1([], Final, _) -> Final.

%% movetoList0 - takes the even position numbers.
movetoList0(List) -> movetoList0(List, [], 0).
movetoList0([Head|Tail], Final, K) -> case K rem 2 of
                                        0 -> movetoList0(Tail, [Head|Final], K+1);
                                        1 -> movetoList0(Tail, Final, K+1)
                                      end;
movetoList0([], Final, _) -> Final.
%%-------------------------------------------
%%-------------------------------------------
%%-------------------------------End of mSort---------------------------

%% qSort - take the first number as pivot.
%% Then quick sort on the rest.
qSort([Pivot|Tail]) -> qSort([X || X<- Tail, X<Pivot]) ++ [Pivot] ++ qSort([X || X<- Tail, X>=Pivot]);
qSort([]) -> [].

%%matElemMult
%%Make Row-Row pairs using list comperhension.
%%Then make item-item pairs and multiple to get the result.
matElemMult(MatA, MatB) -> [rowElemMult(RowA, RowB) || {RowA,RowB} <- lists:zip(MatA, MatB) ].
rowElemMult(RowA, RowB) -> [A*B || {A,B} <- lists:zip(RowA, RowB)].



%% filter lists using the lists:Filter function, with input of a fun
%% that will return true if we want the item to remain in the returned List.
filter_g(List, Filter) when Filter == numbers -> lists:filter(fun(X) -> (true=/=is_number(X)) end, List);
filter_g(List, _) -> lists:filter(fun(X) -> (true=/=is_atom(X)) end, List).


filter_p(List, numbers) -> lists:filter(fun(X) -> (true=/=is_number(X)) end, List);
filter_p(List, atoms) -> lists:filter(fun(X) -> (true=/=is_atom(X)) end, List).
%% can we use lists:filter?

even([Head|Tail]) when Head rem 2 == 0 -> [Head| even(Tail)];
even([_|Tail]) -> even(Tail);
even([]) -> [].



fiboR(0) -> 0;
fiboR(1) -> 1;
fiboR(N) -> fiboR(N-1)+fiboR(N-2).

fiboT(N) -> fiboT(N,0, 1).
fiboT(0,Sum, _) -> Sum;
fiboT(N, Sum, NextSum ) -> fiboT(N-1, NextSum , Sum + NextSum).

%% The Tail recursion is much faster.
%% The calculations happen once each time for the N'th fibonacci.