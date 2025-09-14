-module(dijkstra).
-export([entry/2, replace/4, update/4]).

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
    New = {Node, N, Gateway},
    {Res, Inserted} = lists:foldr(fun(E = {_, Dist, _}, {Acc, Ins}) ->
        case {Ins, Dist < N} of
            {false, true} -> {[E, New | Acc], true};
            _ -> {[E | Acc], Ins}
        end
    end, {[], false}, Cleaned),
    case Inserted of
        true ->
            Res;
        false ->
            [New | Res]
    end.

update(Node, N, Gateway, Sorted) ->
    C = entry(Node, Sorted),
    if
        N < C -> 
            replace(Node, N, Gateway, Sorted); 
        true -> 
            Sorted
    end.