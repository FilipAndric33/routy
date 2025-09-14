-module(map).
-export ([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    Map = [],
    Map.

update(Node, Links, Map) ->
    lists:keydelete(Node, 1, Map),
    Map ++ [{Node, Links}].

reachable(Node, Map) ->
    case Map of
        [{Node, L} | _] -> 
            L;
        [_ | _] ->
            [];
        [] -> []
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
