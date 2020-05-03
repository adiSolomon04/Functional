-module(run).
-author("adisolo").

%% API
-export([run/1, perms/1]).

run(Which) ->
  %%Map = #{zero => exf:new_node(zero), one => exf:new_node(one), x1=> false, x2=>true},%exf:build_map_vars(BoolFunc),
  case Which of
    1 -> Ordering = [num_of_nodes, num_of_leafs, tree_height], lists:map(fun(X) -> get_time_trees(X) end, Ordering);
    Input -> ListPerms = lists:map(fun(X) ->lists:zip([x1,x2,x3,x4],X) end, get_all_perms_bool()),
      case Input of
        2.1 -> Tree = exp_to_bdd(num_of_nodes), {num_of_nodes, lists:map(fun(X) -> get_time_calc(Tree, X) end, ListPerms)};
        2.2 -> Tree = exp_to_bdd(num_of_leafs), {num_of_leafs, lists:map(fun(X) -> get_time_calc(Tree, X) end, ListPerms)};
        2.3 -> Tree = exp_to_bdd(tree_height), {tree_height, lists:map(fun(X) -> get_time_calc(Tree, X) end, ListPerms)}
      end
  end.

%% run:run({'or',{x1,{'not',x2}}}, 2).
%%
%%run:run({'or',{{'and',{{'and',{a,b}},c}},{'and',{{'not',a},{'not',c}}}}}, 2).


get_time_trees(Ordering) ->
  BoolFunction ={'or',{{'or',{{'and',{{'and',{x1,{'not',x2}}},x3}},{'not',
    {'and',{{'and',{{'or',{{'not',x4},x2}},{'not',x3}}},x1}}}}},{'not',{'and',{x1,x4}}}}},
  B = erlang:timestamp(), V = exf:exp_to_bdd(BoolFunction, Ordering), A = erlang:timestamp(), {{timer:now_diff(A,B), Ordering}, V}.

get_time_calc(Tree, Perm) ->
  B = erlang:timestamp(), V = exf:solve_bdd(Tree, Perm), A = erlang:timestamp(), {{timer:now_diff(A,B), Perm}, V}.

get_all_perms_bool() -> sets:to_list(sets:from_list([[false,false,false,false]]++[[true,true,true,true]]++
  perms([true,false,false,false])++
  perms([true,true,true,false])++perms([true,true,false,false]))).
%{'or',{{'or',{{'and',{{'and',{x1,{'not',x2}}},x3}},{'not',{'and',{{'and',{{'or',{{'not',x4},x2}},{'not',x3}}},x1}}}}},{'not',{'and',{x1,x4}}}}}

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

exp_to_bdd(Ordering) ->
  BoolFunction ={'or',{{'or',{{'and',{{'and',{x1,{'not',x2}}},x3}},{'not',
    {'and',{{'and',{{'or',{{'not',x4},x2}},{'not',x3}}},x1}}}}}, {'not',{'and',{x1,x4}}}}},
  exf:exp_to_bdd(BoolFunction, Ordering).
