-module(hist).
-export([new/1, update/3]).

new(Name) ->
    [{Name, inf}].

update(Node, N, History) -> 
    case lists:keyfind(Node, 1, History) of
        false -> 
            Updated = [{Node, N} | History],
            {new, Updated};
        {Node, Last} when N =< Last ->
            old;
        {Node, Last} when N > Last ->
            Updated = lists:keyreplace(Node, 1, History, {Node, N}),
            {new, Updated}
    end.

