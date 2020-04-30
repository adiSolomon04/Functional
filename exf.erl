-module(exf).
-author("Adi Solo").

%% API
-export([exp_to_bdd/2]).

-type tree()::{'treeNode', Name::atom(), Value::integer(),  Right::tree(), Left::tree(), NumNodes::integer(), NumLeaves::integer(), Height::integer()}.

exp_to_bdd(BoolFunc, Ordering) ->
  %%BuiltBoolFunc = buildFunc(BoolFunc),
  Map = buildMapOfVars(BoolFunc),
  ListVars = maps:keys(Map),
  AllPermsVars = perms(ListVars),%%lib_misc:perms
  ListTrees = [bddTree(Map, X)||X<- ListVars],
  bddTreeMax = lists:.


buildMapOfVars(BoolFunc) -> Map = #{one => tree(one, 1, 'nil', 'nil', 1, 1, 0), zero => tree(zero, 0, 'nil', 'nil', 1, 1, 0)}, getVars(BoolFunc, Map).

getVars({'not', Arg}, Map) -> if is_tuple(Arg) -> getVars(Arg, Map);
                               is_atom(Arg) -> Map#{Arg=>0};
                               true -> errorInFunctionBuild_not
                             end;
getVars({'or', {Arg1, Arg2}}, Map) -> if is_tuple(Arg1), is_tuple(Arg2) -> maps:merge(getVars(Arg1, Map),getVars(Arg2, Map));
                                        is_atom(Arg1), is_tuple(Arg2)  -> getVars(Arg2, Map)#{Arg1 => 0};
                                        is_atom(Arg2), is_tuple(Arg1)  -> getVars(Arg1, Map)#{Arg2 => 0};
                                        is_atom(Arg1), is_atom(Arg2) -> Map#{Arg1=>0, Arg2=>0};
                                        true -> errorInFunctionBuild_or
                                      end;

getVars({'and', {Arg1, Arg2}}, Map) -> if is_tuple(Arg1), is_tuple(Arg2) -> maps:merge(getVars(Arg1, Map),getVars(Arg2, Map));
                                         is_atom(Arg1), is_tuple(Arg2)  -> getVars(Arg2, Map)#{Arg1 => 0};
                                         is_atom(Arg2), is_tuple(Arg1)  -> getVars(Arg1, Map)#{Arg2 => 0};
                                         is_atom(Arg1), is_atom(Arg2) -> Map#{Arg1=>0, Arg2=>0};
                                         true -> errorInFunctionBuild_or
                                       end;
getVars(_, _) -> errorInBoolFunction_Tuple.


buildFunc({'not', Arg}, M) -> if is_tuple(Arg) -> fun_not(buildFunc(Arg, M));
                                is_atom(Arg) -> fun_not(maps:get(Arg, M));
                                true -> errorInFunctionBuild_not
                              end;
buildFunc({'or', {Arg1, Arg2}}, M) -> if is_tuple(Arg1), is_tuple(Arg2) -> fun_or(buildFunc(Arg1, M), buildFunc(Arg2, M));
                                        is_atom(Arg1), is_tuple(Arg2)  -> fun_or(maps:get(Arg1, M), buildFunc(Arg2, M));
                                        is_atom(Arg2), is_tuple(Arg1)  -> fun_or(maps:get(Arg2, M), buildFunc(Arg1, M));
                                        is_atom(Arg1), is_atom(Arg2) -> fun_or(maps:get(Arg1, M), maps:get(Arg1, M));
                                        true -> errorInFunctionBuild_or
                                      end;

buildFunc({'and', {Arg1, Arg2}}, M) -> if is_tuple(Arg1), is_tuple(Arg2) -> fun_and(buildFunc(Arg1, M), buildFunc(Arg2, M));
                                        is_atom(Arg1), is_tuple(Arg2)  -> fun_and(maps:get(Arg1, M), buildFunc(Arg2, M));
                                        is_atom(Arg2), is_tuple(Arg1)  -> fun_and(maps:get(Arg2, M), buildFunc(Arg1, M));
                                        is_atom(Arg1), is_atom(Arg2) -> fun_and(maps:get(Arg1, M), maps:get(Arg1, M));
                                        true -> errorInFunctionBuild_and
                                      end;
buildFunc(_, _) -> errorInFunctionBuild_Tuple.

%% Boolean functions
fun_not(Arg) ->
  if Arg==1 -> 0;
    true -> 1
  end.

fun_or(Arg1, Arg2) ->
  if Arg1==1;Arg2==1 ->1;
    true -> 0
  end.

fun_and(Arg1, Arg2) ->
  if Arg1==1,Arg2==1 ->1;
    true -> 0
  end.

bddTree(Map, [Head|Perm]) -> bddTree(Map, Perm, newTreeNode(Head, 'nil')).

bddTree(Map, Perm, Tree) -> .

newTreeNode(Name, Value, Left, Right) -> {'treeNode', Name, Value,  Left, Right, 1, 1, 0}.