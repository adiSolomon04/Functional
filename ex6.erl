-module(ex6).
-author("adisolo").

%% API
-export([songList/1, songGen/3]).


songList([]) -> [];
songList(Songs) -> G = digraph:new(),
  lists:foreach(fun(X) -> digraph:add_vertex(G,X) end, Songs),
  lists:foreach(fun(X) -> lists:foreach(fun([Head1|Tail])->
                                            case lists:last(X) of
                                              Head1 -> digraph:add_edge(G, X, [Head1|Tail]);
                                              _ -> nothing
                                            end
                                        end, Songs)
                end, Songs),
  io:format("number of edges: ~p~n", [digraph:no_edges(G)]),G.

songGen(G, Start, End) -> VerS = digraph:vertex(G,Start), VerE = digraph:vertex(G,End),
                          if
                            VerS==false; VerE==false-> error_in_graph;
                            true -> digraph:get_short_path(G, Start, End)
                          end.