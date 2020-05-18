-module(ex6).
-author("adisolo").

%% API
-export([songList/1]).


songList([]) -> [];
songList(Songs) -> G = digraph:new(),
  lists:foreach(fun(X) ->
  lists:foreach(fun(X) -> lists:foreach(fun([Head1|Tail])->
                                            case lists:X of
                                              [Head1|_] -> digraph:add_edge(G, X, [Head1|Tail])
                                            end


                                        end, Songs)end, Songs), G.


