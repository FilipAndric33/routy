-module(dijkstra).
-import(map, [all_nodes/1]).
-export([table/2, route/2]).

entry(Node, Sorted) ->
    lists:foldl(fun({A, B, _}, Acc)-> 
            case A of
                Node ->
                    if
                        Acc == 0 ->
                            B;
                        Acc > B ->
                            B;
                        true ->
                            Acc
                    end;
                _ -> 
                    Acc
            end
        end, 0, Sorted).

replace(Node, N, Gateway, Sorted) ->
    Cleaned = lists:keydelete(Node, 1, Sorted),
    lists:sort(fun({_, A, _}, {_, B, _}) -> A =< B end, Cleaned ++ [{Node, N, Gateway}]).

update(Node, N, Gateway, Sorted) ->
    C = entry(Node, Sorted),
    if
        N < C -> 
            replace(Node, N, Gateway, Sorted); 
        true -> 
            Sorted
    end.

iterate([], _, Table) -> Table;
iterate([{_, inf, _} | _], _, Table) -> Table;
iterate([{Node, D, Gateway} | Rest], Map, Table) ->  
        Neighbors = case lists:keyfind(Node, 1, Map) of
            {Node, L} -> L;
            false -> []
        end,
        New = lists:foldl(fun(X, Acc) -> 
            update(X, D + 1, Gateway, Acc)
        end, Rest, Neighbors),
        iterate(New, Map, Table ++ [{Node, Gateway}]).

table(Gateways, Map) ->
    AllNodes = all_nodes(Map),
    Sorted = lists:map(fun(X) -> {X, inf, unknown} end, AllNodes),
    New = lists:map(fun(X) -> {X, 0, X} end, Gateways),
    UpdatedSorted = lists:foldl(fun({A, B, C}, Acc) ->
            update(A, B, C, Acc)
        end, Sorted, New),
    UpdatedSorted,
    iterate(UpdatedSorted, Map, []). 

route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        {_, A} -> {ok, A};
        false -> notfound
    end.