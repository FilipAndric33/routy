-module(routy).
-import(interface, [new/0]).
-import(map, [new/0]).
-import(hist, [new/1]).
-import(dijkstra, [table/2]).
-export([start/2]).

start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = interface:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).