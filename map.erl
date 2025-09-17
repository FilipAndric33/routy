-module(map).
-export ([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    Map = [],
    Map.

update(Node, Links, Map) ->
    New = lists:keydelete(Node, 1, Map),
    New ++ [{Node, Links}].

reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {Node, L} -> L;
        false -> []
    end.

all_nodes(Map) ->
    lists:foldl(fun({Node, L}, Acc) ->
            New = [Node] ++ L,
            lists:foldl(fun(X, A) ->
                    case lists:member(X, A) of
                        true -> A;
                        false -> A ++ [X]
                    end
                end, Acc, New)
        end, [], Map).
