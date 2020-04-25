-module(ex2_207859612).
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

%%------------------------
%%another version of deleteKelem Function - with reverse

%deleteKelem([Head|Tail], Elem) -> deleteKelem(Tail, Elem, [], Head);
%deleteKelem([], _) -> [].

%%deleteKelem/4
%deleteKelem([Head|Tail], Elem, List, Elem) -> deleteKelem(Tail, Elem, List, Head);
%deleteKelem([Head|Tail], Elem, List, NotElem) -> deleteKelem(Tail, Elem, [NotElem|List], Head);
%deleteKelem([], Elem, List, Elem) -> exercise2:reverse(List);
%deleteKelem([], _, List, NotElem) -> exercise2:reverse([NotElem|List]).
%%------------------------


%%addKelem Function
addKelem(List, 1, Elem) -> [Elem|List];
addKelem([Head|Tail], K, Elem) -> [Head|addKelem(Tail,K-1, Elem)].
%%------------------


%%union Function
union([Head|Tail], List2) -> union(Tail, Head, List2);
union([], List2) -> List2.

%%union/3
union([Head|Tail], Elem, List2) -> [Elem|union(Tail, Head, ex2_207859612:deleteKelem(List2, Elem))];
union([], Elem, List2) -> [Elem|ex2_207859612:deleteKelem(List2, Elem)].
%%------------------


