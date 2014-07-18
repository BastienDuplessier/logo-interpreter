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
run(_) -> true.

% Basic commands
run_instruction({repeat, {int, _, Times}, Instructions}) -> run_instruction({repeat, Times, Instructions});
run_instruction({repeat, 0, _}) -> true;
run_instruction({repeat, Times, Instructions}) ->
    run(Instructions),
    NewTimes = Times - 1,
    run_instruction({repeat, NewTimes, Instructions});
run_instruction({Symbol, ArgumentList}) ->
    ComputedArguments = compute_arguments(ArgumentList),
    Drawer = drawer(),
    Drawer:do(Symbol, ComputedArguments);
% Default : error
run_instruction(Instruction) ->
    io:format("Bad Instruction :  ~p !\n", [Instruction]).

compute_arguments(List) -> compute_arguments(List, []).
compute_arguments([H|T], Result) ->
    Argument = compute_aexpr(H),
    compute_arguments(T, [Argument|Result]);
compute_arguments([], Result) -> Result.

compute_aexpr({rand, [Expr]}) ->
    {int, ComputedExpr} = compute_aexpr(Expr),
    {int, random:uniform(ComputedExpr)};
compute_aexpr({angle}) ->
    Drawer = drawer(),
    Drawer:angle();
compute_aexpr({int, _, Value}) -> {int, Value};
compute_aexpr({Operator, {A, B}}) ->
    {int, ComputedA} = compute_aexpr(A),
    {int, ComputedB} = compute_aexpr(B),
    {int, compute(Operator, ComputedA, ComputedB)}.


compute(plus, A, B) -> A + B;
compute(minus, A, B) -> A - B;
compute(multiply, A, B) -> A * B;
compute(divide, A, B) -> trunc(A / B).

drawer() -> logofakedrawer.
