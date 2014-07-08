-module(logofakedrawer).
-export([do/2, av/1, td/1, tg/1, rec/1, fpos/1, fcap/1, ve/1,
	 mt/1, ct/1, lc/1, bc/1, fcc/1]).

do(Symbol, ArgumentList) -> logofakedrawer:Symbol(ArgumentList).
% Basic commands
av([X]) ->
    io:format("Going Forward - ~p !\n", [X]).
td([X]) ->
    io:format("Turning right - ~p !\n", [X]).
tg([X]) ->
    io:format("Turning left - ~p !\n", [X]).
rec([X]) ->
    io:format("Going Backward - ~p !\n", [X]).
fpos([X, Y]) ->
    io:format("Positioning - ~p ~p !\n", [X, Y]).
fcap(X) ->
    io:format("Setting angle - ~p !\n", [X]).
ve([]) ->
    io:format("Cleaning screen\n").
mt([]) -> 
    io:format("Show turtle\n").
ct([]) ->
    io:format("Hide turtle\n").
lc([]) ->
    io:format("Lift pencil\n").
bc([]) ->
    io:format("Lower pencil\n").
fcc([X]) ->
    io:format("Change color to ~p\n", [X]).
