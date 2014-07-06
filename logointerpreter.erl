-module(logointerpreter).
-export([test/0, execute/1]).

test() ->
    execute("VE FPOS [ 0 0 ] BC AV 100 TD 90 AV 100 TD 90 AV 100").

execute(String) ->
    {ok, Tokens, _} = logoscanner:string(String),
    {ok, ParseTree} = logoparser:parse(Tokens),
    run(ParseTree).

run([Instruction|Rest]) ->
    run_instruction(Instruction),
    run(Rest);
run([]) ->
    true;
run(_) -> true.

% Basic commands
run_instruction({av, [Argument]}) ->
    io:format("Going Forward - ~p !\n", [Argument]);
run_instruction({td, [Argument]}) ->
    io:format("Turning right - ~p !\n", [Argument]);
run_instruction({tg, [Argument]}) ->
    io:format("Turning left - ~p !\n", [Argument]);
run_instruction({rec, [Argument]}) ->
    io:format("Going Backward - ~p !\n", [Argument]);
run_instruction({fpos, Arguments}) ->
    io:format("Positioning - ~p !\n", [Arguments]);
run_instruction({fcap, [Argument]}) ->
    io:format("Setting angle - ~p !\n", Argument);
run_instruction({ve, []}) ->
    io:format("Cleaning screen\n");
run_instruction({mt, []}) ->
    io:format("Show turtle\n");
run_instruction({ct, []}) ->
    io:format("Hide turtle\n");
run_instruction({lc, []}) ->
    io:format("Lift pencil\n");
run_instruction({bc, []}) ->
    io:format("Lower pencil\n");
run_instruction({fcc, [Argument]}) ->
    io:format("Change color to ~p\n", [Argument]);
% Default : error
run_instruction(Instruction) ->
    io:format("Bad Instruction :  ~p !\n", [Instruction]).

