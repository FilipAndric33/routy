-module(map).
-export ([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    case whereis(map) of
        undefined -> ok;
        Pid ->  exit(Pid, kill)
    end,
    register(map, spawn(fun() -> listen(Map = []) end)),
    [].
    
listen(Map) ->
    receive
        {update, [Res]} ->
            listen(Map ++ [Res])
    end.



update(Node, Links, Map) ->
    lists:keydelete(Node, 1, Map),
    %map ! {update, [{Node, Links}]},
    {Node, Links}.

reachable(Node, Map) ->
    case Map of
        [{Node, L} | _] -> 
            L;
        [_ | _] ->
            [];
        [] -> []
    end.

all_nodes(Map) ->
    case Map of
        [{Node, L} | _] ->
            [Node] ++ L;
        [] -> 
            []
    end.

