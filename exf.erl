-module(exf).
%%-author("Adi Solo").

%% API
-export([exp_to_bdd/2, solve_bdd/2]).

%% Record - node
%% functions to create, get and update nodes
-record(node, {name, value = nil, right = nil, left = nil, numNodes = 1, numLeafs = 1, height = 0}).

%% Create new node
new_node(Name) -> #node{name = Name}.

get_name(Record) -> Record#node.name.
get_children(Record) -> {Record#node.right, Record#node.left}.
get_data(Record) -> {Record#node.numNodes, Record#node.numLeafs, Record#node.height}.

get_right(Record) -> Record#node.right.
get_left(Record) -> Record#node.left.

%% Setter
update_node(Record, Right, Left, NumNodes, NumLeaves, Height) ->
  Record#node{left = Left, right = Right, numNodes = NumNodes, numLeafs = NumLeaves, height = Height}.

%%-----------------------------------exp_to_bdd----------------------------------
exp_to_bdd(BoolFunc, Ordering) ->
  Map = get_vars(BoolFunc, #{}),
  ListVars = maps:keys(Map),
  ListTrees = lists:map(fun(X) -> bdd_tree(maps:merge(Map,#{one => new_node(one), zero => new_node(zero)}) , X, BoolFunc) end, perms(ListVars)),
  ListNeededData = needed_data(ListTrees, Ordering),
  MinData = lists:min(ListNeededData),
  find_most_efficient(ListTrees, ListNeededData, MinData).


%% Find the tree with the most efficient Ordering.
find_most_efficient([Tree|_], [Data|_], MinData) when Data==MinData -> Tree;
find_most_efficient([_|ListTrees], [_|ListNeededData], MinData) ->
  find_most_efficient(ListTrees, ListNeededData, MinData).

%% Get the ordering Needed Data
needed_data(ListTrees, Ordering) -> case Ordering of
                                     num_of_nodes -> lists:map(fun(X) -> #node{numNodes = Data} = X, Data end, ListTrees);
                                     num_of_leafs -> lists:map(fun(X) -> #node{numLeafs = Data} = X, Data end, ListTrees);
                                     tree_height -> lists:map(fun(X) -> #node{height = Data} = X, Data end, ListTrees);
                                      _ -> errorInOrderingInput
                                   end.

%% Input all variables to the Map,
%% Has two unique variables - 1, 0.
build_map_vars(BoolFunc) -> Map = #{one => new_node(one), zero => new_node(zero)}, get_vars(BoolFunc, Map).

% Every found atom is entered to the Map
get_vars({'not', Arg}, Map) -> if is_tuple(Arg) -> get_vars(Arg, Map);
                               is_atom(Arg) -> Map#{Arg=>false};
                               true -> errorInFunctionBuild_not
                             end;
get_vars({'or', {Arg1, Arg2}}, Map) -> if is_tuple(Arg1), is_tuple(Arg2) -> maps:merge(get_vars(Arg1, Map), get_vars(Arg2, Map));
                                        is_atom(Arg1), is_tuple(Arg2)  -> maps:put(Arg1, false, get_vars(Arg2, Map));%%getVars(Arg2, Map)#{Arg1 => 0};
                                        is_atom(Arg2), is_tuple(Arg1)  -> maps:put(Arg2, false, get_vars(Arg1, Map));
                                        is_atom(Arg1), is_atom(Arg2) -> Map#{Arg1=>false, Arg2=>false};
                                        true -> errorInFunctionBuild_or
                                      end;

get_vars({'and', {Arg1, Arg2}}, Map) -> if is_tuple(Arg1), is_tuple(Arg2) -> maps:merge(get_vars(Arg1, Map), get_vars(Arg2, Map));
                                         is_atom(Arg1), is_tuple(Arg2)  -> maps:put(Arg1, false, get_vars(Arg2, Map));
                                         is_atom(Arg2), is_tuple(Arg1)  -> maps:put(Arg2, false, get_vars(Arg1, Map));
                                         is_atom(Arg1), is_atom(Arg2) -> Map#{Arg1=>false, Arg2=>false};
                                         true -> errorInFunctionBuild_or
                                       end;

get_vars(_, _) -> errorInBoolFunction_Tuple.

%% Calculates the boolean function.
%% uses The tuple values in the map.
bool_func_calc({'not', Arg}, M) ->
  if is_tuple(Arg) -> not bool_func_calc(Arg, M);
    is_atom(Arg) -> not maps:get(Arg, M);
    true -> errorInFunctionBuild_not
  end;
bool_func_calc({'or', {Arg1, Arg2}}, M) ->
  (if is_tuple(Arg1) -> bool_func_calc(Arg1, M);
     is_atom(Arg1) -> maps:get(Arg1, M);
     true -> error_bool_func_calc_tuple
   end)
    or
    (if is_tuple(Arg2) -> bool_func_calc(Arg2, M);
       is_atom(Arg2) -> maps:get(Arg2, M);
       true -> error_bool_func_calc_tuple
     end);
bool_func_calc({'and', {Arg1, Arg2}}, M) ->
  (if is_tuple(Arg1) -> bool_func_calc(Arg1, M);
     is_atom(Arg1) -> maps:get(Arg1, M);
     true -> error_bool_func_calc_tuple
   end)
    and
    (if is_tuple(Arg2) -> bool_func_calc(Arg2, M);
       is_atom(Arg2) -> maps:get(Arg2, M);
       true -> error_bool_func_calc_tuple
     end).

%% first call to bddTree
bdd_tree(Map, [Head|Perm], BoolFunction) -> bdd_tree(Map, Perm, new_node(Head), BoolFunction);
bdd_tree(_, [], _) -> errorPermListEmpty.

% second bddTree, recursive function
bdd_tree(Map, [Head|Perm], Tree, BoolFunction) ->
  %% The way Down of the tree
  Right = bdd_tree(Map#{get_name(Tree):=true}, Perm, new_node(Head), BoolFunction),
  Left =  bdd_tree(Map#{get_name(Tree):=false}, Perm, new_node(Head), BoolFunction),
  %% On the way up from recursive call
  IsEqual = is_sub_equal(Right, Left),
  if IsEqual -> Right;
    true ->
      {NumNodesR, NumLeavesR, HeightR} = get_data(Right),
      {NumNodesL, NumLeavesL, HeightL} = get_data(Left),
      update_node(Tree, Right, Left,
        NumNodesR+NumNodesL+1, NumLeavesR+NumLeavesL, max(HeightR, HeightL)+1)
  end;

bdd_tree(Map, [], Tree, BoolFunction) ->
  Right = get_leaf(Map, bool_func_calc(BoolFunction, Map#{get_name(Tree):=true})),
  Left =  get_leaf(Map, bool_func_calc(BoolFunction, Map#{get_name(Tree):=false})),
  IsEqual = is_sub_equal(Right, Left),
  if IsEqual -> Right;
    true ->
      {NumNodesR, NumLeavesR, HeightR} = get_data(Right),
      {NumNodesL, NumLeavesL, HeightL} = get_data(Left),
  update_node(Tree, Right, Left,
    NumNodesR+NumNodesL+1, NumLeavesR+NumLeavesL, max(HeightR, HeightL)+1)
  end.

%% Checks Recursively if two sub trees are equal.
is_sub_equal(SubT1, SubT2) when is_record(SubT1, node), is_record(SubT2, node) ->
  Name1 = get_name(SubT1), Name2 = get_name(SubT2)
  , {Right1, Left1} = get_children(SubT1), {Right2, Left2} = get_children(SubT2),
  if Name1=/=Name2 -> false;
     true-> is_sub_equal(Right1,Right2) and is_sub_equal(Left1,Left2)
  end;
is_sub_equal('nil', 'nil') -> true;
is_sub_equal(_,_) -> false.

%% Gets one of the initialized variable nodes - 1 or 0.
get_leaf(Map, Calc) ->
  maps:get(
    case Calc of
       true -> one;
       _ -> zero
    end,
    Map
  ).

%%Make all permutations of a list.
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

%%-----------------------------------solve_bdd----------------------------------
solve_bdd(BddTree, ListVars) ->
  find_res(BddTree, maps:from_list(
    lists:map(fun({Arg,X}) -> case X of
                                1 -> {Arg, true};
                                0 ->{Arg, false};
                                _ -> {Arg, X}
                              end
              end,
      ListVars)++
    [{one, true}, {zero, false}])).

find_res(BddTree, Map) when is_record(BddTree, node)-> %%Val = maps:get(get_name(BddTree)), Name = get_name(BddTree),
  case get_name(BddTree) of
    Name when Name == one orelse Name== zero -> maps:get(get_name(BddTree), Map);
    _ ->
      find_res(
        case maps:get(get_name(BddTree), Map) of
          Val when Val -> get_right(BddTree);
          _ -> get_left(BddTree)
        end,
        Map
      )
  end;
find_res(_, _) -> error.


tree_only(Map, [Head|Perm], BoolFunction) -> tree_only(Map, Perm, new_node(Head), BoolFunction);
tree_only(_, [], _) -> errorPermListEmpty.

% second bddTree, recursive function
tree_only(Map, [Head|Perm], Tree, BoolFunction) ->
  %% The way Down of the tree
  Right = tree_only(Map#{get_name(Tree):=true}, Perm, new_node(Head), BoolFunction),
  Left =  tree_only(Map#{get_name(Tree):=false}, Perm, new_node(Head), BoolFunction),
  %% On the way up from recursive call
  if
    true ->
      {NumNodesR, NumLeavesR, HeightR} = get_data(Right),
      {NumNodesL, NumLeavesL, HeightL} = get_data(Left),
      update_node(Tree, Right, Left,
        NumNodesR+NumNodesL+1, NumLeavesR+NumLeavesL, max(HeightR, HeightL)+1)
  end;

tree_only(Map, [], Tree, BoolFunction) ->
  Right = get_leaf(Map, bool_func_calc(BoolFunction, Map#{get_name(Tree):=true})),
  %newNode('leaf', calcFunc(Map#{getName(Tree):=1})), %%connect to one, zero nodes
  Left =  get_leaf(Map, bool_func_calc(BoolFunction, Map#{get_name(Tree):=false})),
  %newNode('leaf', calcFunc(Map#{getName(Tree):=0})),
  if
    true ->
      {NumNodesR, NumLeavesR, HeightR} = get_data(Right),
      {NumNodesL, NumLeavesL, HeightL} = get_data(Left),
      update_node(Tree, {Right, Map#{get_name(Tree):=true}}, {Left, Map#{get_name(Tree):=false}},
        NumNodesR+NumNodesL+1, NumLeavesR+NumLeavesL, max(HeightR, HeightL)+1)
  end.
