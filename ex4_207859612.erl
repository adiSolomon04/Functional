-module(ex4_207859612).

%% API

-export([flatten/1, smaller/2, replace/3, mapSub/2]).


%%flatten(List) -> lists:map(fun(X) -> flattenElem(X) end, List).

%%flattenElem(X) when is_list(X) -> lists:map(flatten(X), X);
%%flattenElem(X) -> X.


%% If an element is a list - flatten it recursively.
%% If it isnt a list - put is in Acc
flatten(List) when is_list(List) -> lists:reverse(flatten(List, []));
flatten(_) -> error.

flatten([Head|Tail], Acc) when is_list(Head) -> flatten(Tail, flatten(Head, Acc));
flatten([Head|Tail], Acc) -> flatten(Tail, [Head|Acc]);
flatten([], Acc) -> Acc.

%% If it is smaller lambda puts true, else it puts false.
smaller(List, Thr) when is_list(List) -> lists:map(fun(X) -> is_smaller(X, Thr) end, List);
smaller(_, _) -> error.

is_smaller(X, Thr) when X=<Thr -> true;
is_smaller(_, _) -> false.

%%Use macro to check if am element is equal and replace if needed.
replace(List, Old, New) when is_list(List) -> lists:map(fun(X) -> replaceElem(X, Old, New) end, List);
replace(_, _, _) -> error.

replaceElem(X, Old, New) when Old == X -> New;
replaceElem(X, _, _) -> X.

%% use macro to subtract it.
mapSub(List, Arg) when is_list(List), is_number(Arg) -> lists:map(fun(X) -> elemSub(X, Arg) end, List);
mapSub(List, Arg) when  is_list(List), is_list(Arg), length(List)=:=length(Arg)-> lists:map(fun(X) -> listsSub(X) end, mergeLists(List, Arg));
mapSub(List, []) when  is_list(List) -> List;
mapSub(_, _) -> lenError.

%%subs a number with arg.
elemSub(X, Arg) when is_number(X) ->X-Arg;
elemSub(X, Arg) -> error.

%%subs the separated elements from two lists
listsSub([Head1,Head2|_]) when is_number(Head1),is_number(Head2) ->Head1-Head2;
listsSub(_) -> error.

%%Moves two lists into one list with lists of separated elements
mergeLists([Head|Tail], [HeadArg|TailArg]) -> [[Head,HeadArg]|mergeLists(Tail, TailArg)];
mergeLists([], []) -> [].
