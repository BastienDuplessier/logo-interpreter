-module(logointerpreter).
-export([test/0, execute/1]).

test() ->
    execute("AV (23 + 43 * (34 + 54)) / 10").

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
    io:format("Going Forward - ~p !\n", [compute_aexpr(Argument)]);
run_instruction({td, [Argument]}) ->
    io:format("Turning right - ~p !\n", [compute_aexpr(Argument)]);
run_instruction({tg, [Argument]}) ->
    io:format("Turning left - ~p !\n", [compute_aexpr(Argument)]);
run_instruction({rec, [Argument]}) ->
    io:format("Going Backward - ~p !\n", [compute_aexpr(Argument)]);
run_instruction({fpos, [X, Y]}) ->
    io:format("Positioning - ~p ~p !\n", [compute_aexpr(X), compute_aexpr(Y)]);
run_instruction({fcap, [Argument]}) ->
    io:format("Setting angle - ~p !\n", compute_aexpr(Argument));
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
    io:format("Change color to ~p\n", [compute_aexpr(Argument)]);
% Default : error
run_instruction(Instruction) ->
    io:format("Bad Instruction :  ~p !\n", [Instruction]).


compute_aexpr({{Operator}, A, B}) ->
    {int, ComputedA} = compute_aexpr(A),
    {int, ComputedB} = compute_aexpr(B),
    {int, trunc(compute(Operator, ComputedA, ComputedB))};
compute_aexpr(Value) -> Value.

compute(plus, A, B) -> A + B;
compute(minus, A, B) -> A - B;
compute(multiply, A, B) -> A * B;
compute(divide, A, B) -> A / B.
