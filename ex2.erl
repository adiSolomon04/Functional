-module(ex2).
%% API
-export([findKelem/2, reverse/1, deleteKelem/2, addKelem/3, union/2]).

%%findKelem Function
findKelem(List,K) -> findKelem(List, K, 1).

%%findKelem/3
findKelem([Head|_], K, K) -> Head;
findKelem([_|Tail], K, Count) -> findKelem(Tail, K, Count+1);
findKelem([], _, _) -> notFound.
%%------------------

%%reverse Function
reverse([Head|Tail]) -> reverse(Tail,[Head]);
reverse([]) -> [].

%%reverse/2
reverse([Head|Tail], List) -> reverse(Tail,[Head|List]);
reverse([], List) -> List.
%%------------------


%%deleteKelem Function - using reverse
deleteKelem([Elem|Tail], Elem) -> deleteKelem(Tail, Elem);
deleteKelem([Head|Tail], Elem) -> [Head|deleteKelem(Tail, Elem)];
deleteKelem([], _) -> [].
%%------------------

%%addKelem Function
addKelem(List, 1, Elem) -> [Elem|List];
addKelem([Head|Tail], K, Elem) -> [Head|addKelem(Tail,K-1, Elem)].
%%------------------

%%union Function
%%union(List1, List2) -> union(List1, List2, []).

%% union - take an element from List1.
%% Each step - delete the element from both lists (so there are no duplications)
%% and add it to the final list.
%% Now do the same only on List2.
union([Head|Tail], List2) -> [Head|union(deleteKelem(Tail, Head), deleteKelem(List2, Head))];
union([], [Head|Tail]) -> [Head|deleteKelem(Tail, Head)];
union([], []) -> [].


%%------------------


