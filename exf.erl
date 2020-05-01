-module(exf).
%%-author("Adi Solo").

%% API
-export([exp_to_bdd/2, solve_bdd/2]).

%% Record - node
%% functions to create, get and update nodes
-record(node, {name, value = 'nil', right = 'nil', left = 'nil', numNodes = 1, numLeafs = 1, height = 0}).

%% Create new node
newNode(Name) -> #node{name = Name}.
newNode(Name,Value) -> #node{name = Name, value = Value}.

%% Getters
getName(Record) -> #node{name = Name} = Record, Name.
getChildren(Record) -> #node{right = Right, left = Left} = Record, {Right, Left}.
getData(Record) -> #node{numNodes = NumNodes, numLeafs = NumLeaves, height = Height} = Record,
  {NumNodes, NumLeaves, Height}.

getRight(Record) -> #node{right = Right} = Record, Right.
getLeft(Record) -> #node{left = Left} = Record, Left.


%% Setter
updateNode(Record, Right, Left, NumNodes, NumLeaves, Height) ->
  Record#node{left = Left, right = Right, numNodes = NumNodes, numLeafs = NumLeaves, height = Height}.

%%todo delete if not needed.
%setChildren(Record, {Right, Left}) -> Record#node{right = Right, left = Left}.

%setData(Record,{NumNodes, NumLeaves, Height}) -> Record#node{numNodes = NumNodes, numLeafs = NumLeaves, height = Height}.

%setLeft(Record, Left) -> Record#node{left = Left}.
%setRight(Record, Right) -> Record#node{right = Right}.
%updateNode(Record, Right, Left) -> Record#node{left = Left, right = Right}.

%%-----------------------------------exp_to_bdd----------------------------------
exp_to_bdd(BoolFunc, Ordering) ->
  Map = buildMapOfVars(BoolFunc),
  ListVars = maps:keys(Map),
  ListTrees = lists:map(fun(X) -> bddTree(Map, X, BoolFunc) end, perms(ListVars)),
  ListNeededData = neededData(ListTrees, Ordering),
  MinData = lists:min(ListNeededData),
  findMostEfficient(ListTrees, ListNeededData, MinData).


%% Find the tree with the most efficient Ordering.
findMostEfficient([Tree|_], [Data|_], MinData) when Data==MinData -> Tree;
findMostEfficient([_|ListTrees], [_|ListNeededData], MinData) ->
  findMostEfficient(ListTrees, ListNeededData, MinData).

%% Get the ordering Needed Data
neededData(ListTrees, Ordering) -> case Ordering of
                                     num_of_nodes -> lists:map(fun(X) -> #node{numNodes = Data} = X, Data end, ListTrees);
                                     num_of_leafs -> lists:map(fun(X) -> #node{numLeafs = Data} = X, Data end, ListTrees);
                                     tree_height -> lists:map(fun(X) -> #node{height = Data} = X, Data end, ListTrees)
                                   end;
neededData(_, _) -> errorInOrderingInput.

%% Input all variables to the Map,
%% Has two unique variables - 1, 0.
buildMapOfVars(BoolFunc) -> Map = #{'one' => newNode('one', 1), 'zero' => newNode('one', 0)}, getVars(BoolFunc, Map).

% Every found atom is entered to the Map
getVars({'not', Arg}, Map) -> if is_tuple(Arg) -> getVars(Arg, Map);
                               is_atom(Arg) -> Map#{Arg=>0};
                               true -> errorInFunctionBuild_not
                             end;
getVars({Arg, {Arg1, Arg2}}, Map) when (Arg=='or' or Arg=='and') ->
                                      if is_tuple(Arg1), is_tuple(Arg2) -> maps:merge(getVars(Arg1, Map),getVars(Arg2, Map));
                                        is_atom(Arg1), is_tuple(Arg2)  -> maps:put(Arg1, 0, getVars(Arg2, Map));%%getVars(Arg2, Map)#{Arg1 => 0};
                                        is_atom(Arg2), is_tuple(Arg1)  -> maps:put(Arg2, 0, getVars(Arg1, Map));
                                        is_atom(Arg1), is_atom(Arg2) -> Map#{Arg1=>0, Arg2=>0};
                                        true -> errorInFunctionBuild_or
                                      end;

%getVars({'and', {Arg1, Arg2}}, Map) -> if is_tuple(Arg1), is_tuple(Arg2) -> maps:merge(getVars(Arg1, Map),getVars(Arg2, Map));
 %                                        is_atom(Arg1), is_tuple(Arg2)  -> getVars(Arg2, Map)#{Arg1 => 0};
  %                                       is_atom(Arg2), is_tuple(Arg1)  -> getVars(Arg1, Map)#{Arg2 => 0};
   %                                      is_atom(Arg1), is_atom(Arg2) -> Map#{Arg1=>0, Arg2=>0};
    %                                     true -> errorInFunctionBuild_or
     %                                  end;

getVars(_, _) -> errorInBoolFunction_Tuple.

%% Calculates the boolean function.
%% uses The tuple values in the map.
boolFuncCalc({'not', Arg}, M) -> if is_tuple(Arg) -> fun_not(boolFuncCalc(Arg, M));
                                is_atom(Arg) -> fun_not(maps:get(Arg, M));
                                true -> errorInFunctionBuild_not
                              end;
boolFuncCalc({'or', {Arg1, Arg2}}, M) -> if is_tuple(Arg1), is_tuple(Arg2) -> fun_or(boolFuncCalc(Arg1, M), boolFuncCalc(Arg2, M));
                                        is_atom(Arg1), is_tuple(Arg2)  -> fun_or(maps:get(Arg1, M), boolFuncCalc(Arg2, M));
                                        is_atom(Arg2), is_tuple(Arg1)  -> fun_or(maps:get(Arg2, M), boolFuncCalc(Arg1, M));
                                        is_atom(Arg1), is_atom(Arg2) -> fun_or(maps:get(Arg1, M), maps:get(Arg1, M));
                                        true -> errorInFunctionBuild_or
                                      end;

boolFuncCalc({'and', {Arg1, Arg2}}, M) -> if is_tuple(Arg1), is_tuple(Arg2) -> fun_and(boolFuncCalc(Arg1, M), boolFuncCalc(Arg2, M));
                                        is_atom(Arg1), is_tuple(Arg2)  -> fun_and(maps:get(Arg1, M), boolFuncCalc(Arg2, M));
                                        is_atom(Arg2), is_tuple(Arg1)  -> fun_and(maps:get(Arg2, M), boolFuncCalc(Arg1, M));
                                        is_atom(Arg1), is_atom(Arg2) -> fun_and(maps:get(Arg1, M), maps:get(Arg1, M));
                                        true -> errorInFunctionBuild_and
                                      end;
boolFuncCalc(_, _) -> errorInFunctionBuild_Tuple.

%% Boolean functions
fun_not(Arg) ->
  if Arg==1 -> 0;
     Arg==0 -> 1;
     true -> error
  end.

fun_or(Arg1, Arg2) ->
  if Arg1==1;Arg2==1 ->1;
     Arg1==0,Arg2==0 ->0;
    true -> error
  end.

fun_and(Arg1, Arg2) ->
  if Arg1==1,Arg2==1 ->1;
     Arg1==0;Arg2==0 ->0;
     true -> error
  end.

%% first call to bddTree
bddTree(Map, [Head|Perm], BoolFunction) -> bddTree(Map, Perm, newNode(Head), BoolFunction);
bddTree(_, [], _) -> errorPermListEmpty.

% second bddTree, recursive function
bddTree(Map, [Head|Perm], Tree, BoolFunction) ->
  %% The way Down of the tree
  Right = bddTree(Map#{getName(Tree):=1}, Perm, newNode(Head), BoolFunction),
  Left =  bddTree(Map#{getName(Tree):=0}, Perm, newNode(Head), BoolFunction),
  %% On the way up from recursive call
  IsEqual = isSubTEqual(Right, Left),
  if IsEqual -> Right;
    true ->
      {NumNodesR, NumLeavesR, HeightR} = getData(Right),
      {NumNodesL, NumLeavesL, HeightL} = getData(Left),
      updateNode(Tree, Right, Left,
        NumNodesR+NumNodesL+1, NumLeavesR+NumLeavesL, max(HeightR, HeightL)+1)
  end;

bddTree(Map, [], Tree, BoolFunction) ->
  Right = getLeaf(Map, boolFuncCalc(BoolFunction, Map#{getName(Tree):=1})),
    %newNode('leaf', calcFunc(Map#{getName(Tree):=1})), %%connect to one, zero nodes
  Left =  getLeaf(Map, boolFuncCalc(BoolFunction, Map#{getName(Tree):=0})),
    %newNode('leaf', calcFunc(Map#{getName(Tree):=0})),
  IsEqual = isSubTEqual(Right, Left),
  if IsEqual -> Right;
    true ->
      {NumNodesR, NumLeavesR, HeightR} = getData(Right),
      {NumNodesL, NumLeavesL, HeightL} = getData(Left),
  updateNode(Tree, Right, Left,
    NumNodesR+NumNodesL+1, NumLeavesR+NumLeavesL, max(HeightR, HeightL)+1)
  end.

%% Checks Recursively if two sub trees are equal.
isSubTEqual(SubT1, SubT2) when is_record(SubT1, node)and is_record(SubT2, node) ->
  Name1 =getName(SubT1), Name2 = getName(SubT2)
  , {Right1, Left1} = getChildren(SubT1), {Right2, Left2} = getChildren(SubT2),
  if Name1=/=Name2 -> false;
     true-> isSubTEqual(Right1,Right2) and isSubTEqual(Left1,Left2)
  end;
isSubTEqual('nil', 'nil') -> true;
isSubTEqual(_,_) -> false.

%% Gets one of the initialized variable nodes - 1 or 0.
getLeaf(Map, Calc) -> if Calc == 1 -> maps:get(one, Map);
                        Calc == 0 -> maps:get(zero, Map);
                        is_atom(Calc) -> Calc %%todo: on error??
                      end.

%%Make all permutations of a list.
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

%%-----------------------------------solve_bdd----------------------------------
solve_bdd(BddTree, ListVars) ->
  findRes(BddTree, maps:from_list(ListVars++[{one, 1}, {zero, 0}])).

findRes(BddTree, Map) -> Val = maps:get(getName(BddTree)), Name = getName(BddTree),
  if (Name=='one' or Name=='zero') -> maps:get(getName(BddTree));
    (Val==1 or Val==true) -> findRes(getRight(BddTree), Map);
    (Val==0 or Val==false) -> findRes(getLeft(BddTree), Map)
  end.

