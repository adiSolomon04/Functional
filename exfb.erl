-module(exfb).
%%-author("Adi Solo").

%% API
-export([exp_to_bdd/2, solve_bdd/2, booleanGenerator/2]).

%% Record - node
%% functions to create, get and update nodes
-record(node, {name, right = nil, left = nil, numNodes = 0, numLeafs = 0, height = 0}).

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
  TimeStart = erlang:timestamp(), %% Time catch
  Map = get_vars(BoolFunc, #{}),  %% Build map of all variables
  ListVars = maps:keys(Map),      %% Get map of keys so we can make permutation.
  %% From all permutations - build trees.
  %% AfterWards - find the most efficient and return it.
  ListTrees = lists:map(fun(X) -> bdd_tree(maps:merge(Map,#{true => new_node(true), false => new_node(false)}) , X, BoolFunc) end, perms(ListVars)),
  ListNeededData = needed_data(ListTrees, Ordering),
  MinData = lists:min(ListNeededData),
  Tree = find_most_efficient(ListTrees, ListNeededData, MinData),
  io:format("total time taken ~f seconds~n", [timer:now_diff(erlang:timestamp(), TimeStart) / 1000000]),
  Tree.




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
      if NumLeavesL+NumLeavesR == 0 -> %% means that both are leafs
        update_node(Tree, Right, Left,
          1, 1, 0);
         true -> update_node(Tree, Right, Left,
        NumNodesR+NumNodesL+1, NumLeavesR+NumLeavesL, max(HeightR, HeightL)+1)
      end
  end;

%% Ended the tree build, starting to go back up
bdd_tree(Map, [], Tree, BoolFunction) ->
  Right = maps:get(bool_func_calc(BoolFunction, Map#{get_name(Tree):=true}), Map),
  Left =  maps:get(bool_func_calc(BoolFunction, Map#{get_name(Tree):=false}),Map),
  IsEqual = is_sub_equal(Right, Left),
  if IsEqual -> Right;
    true ->
  update_node(Tree, Right, Left,
    1, 1, 0)
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

%%Make all permutations of a list.
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

%%-----------------------------------solve_bdd----------------------------------
solve_bdd(BddTree, ListVars) ->
  TimeStart = erlang:timestamp(),         %% Time catch
  Res = find_res(BddTree, maps:from_list( %%make a map of variables
    lists:map(fun({Arg,X}) -> case X of
                                1 -> {Arg, true};
                                0 ->{Arg, false};
                                _ -> {Arg, X}
                              end
              end,
      ListVars)++                         %% Switch 0 and 1 in input List to boolean atoms
    [{true, true}, {false, false}])),     %% add the leafs values to the map
    io:format("total time taken ~f seconds~n", [timer:now_diff(erlang:timestamp(), TimeStart) / 1000000]),
    Res.

find_res(BddTree, Map) when is_record(BddTree, node)->
  case get_name(BddTree) of
    Name when Name == true orelse Name== false -> get_name(BddTree); %% If the record is a leaf - return the result
    _ ->
      find_res(
        case maps:get(get_name(BddTree), Map) of                     %% Go to the Left or Right child accordingly to map variables.
          Val when Val -> get_right(BddTree);
          _ -> get_left(BddTree)
        end,
        Map
      )
  end;
find_res(_, _) -> error.


%%-----------------------------------booleanGenerator----------------------------------
booleanGenerator(NumOfVars, NumOfEquations) ->
  lists:map(fun(_) -> boolean_gen(NumOfVars, NumOfVars) end,  %% Run the function on every place in the list
    lists:seq(1, NumOfEquations)).                            %% Make a list of size NumOfEquations


boolean_gen(NumOfVars, MaxNum) -> case rand:uniform() of                            %% Take a random number
                            Random when Random=<0.2 -> {'not',case NumOfVars of     %% Generate 'not'
                                                                 1 -> list_to_atom(lists:flatten(io_lib:format("x~p", [NumOfVars])));
                                                                 _ -> boolean_gen(NumOfVars, MaxNum)
                                                               end};
                            _ -> {case rand:uniform() of
                                   Random2 when Random2=<0.5 -> 'and';              %% Generate 'or', 'and'
                                   _ -> 'or'
                                 end,case NumOfVars of
                                         1 -> {list_to_atom(lists:flatten(io_lib:format("x~p", [rand:uniform(MaxNum)]))),
                                           list_to_atom(lists:flatten(io_lib:format("x~p", [NumOfVars])))};
                                         _ ->{list_to_atom(lists:flatten(io_lib:format("x~p", [NumOfVars]))),
                                           boolean_gen(NumOfVars-1, MaxNum)}
                                       end
                            }
                          end.