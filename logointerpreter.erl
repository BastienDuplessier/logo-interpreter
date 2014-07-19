-module(logointerpreter).
-export([test/0, execute/1]).

test() ->
    execute("AV (23 + 43 * (34 + 54)) / 10").

execute(String) ->
    {ok, Tokens, _} = logoscanner:string(String),
    {ok, ParseTree} = logoparser:parse(Tokens),
    run(ParseTree, []).

run([Instruction|Rest], Variables) ->
    case run_instruction(Instruction, Variables) of
	{ok, NewVariables} -> run(Rest, NewVariables);
	{error, _} -> io:format("There was an error on ~p\n", [Instruction])
    end;
run(_, Variables) -> {ok, Variables}.


% Repeat
run_instruction({repeat, {int, _, Times}, Instructions}, Variables) -> 
    case run_instruction({repeat, Times, Instructions}, Variables) of
	{ok, _} -> {ok, Variables};
	Error -> Error
    end;
run_instruction({repeat, 0, _}, Variables) -> {ok, Variables};
run_instruction({repeat, Times, Instructions}, Variables) ->
    NewTimes = Times - 1,
    case run(Instructions, Variables) of
	{ok, NewVariables} ->
	    run_instruction({repeat, NewTimes, Instructions}, NewVariables);
	Error -> Error
    end;
% Basic commands
run_instruction({Symbol, ArgumentList}, Variables) ->
    ComputedArguments = compute_arguments(ArgumentList),
    Drawer = drawer(),
    Drawer:do(Symbol, ComputedArguments);
% Default : error
run_instruction(Instruction, _) ->
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


% Variables management
add({Name, Value}, Variables) ->
    NewVariables = remove(Name, Variables),
    [{Name, Value} | NewVariables].

remove(Name, Variables) -> remove(Name, Variables, []).
remove(Name, [{Name, _}| Rest], Variables) -> remove(Name, Rest, Variables);
remove(Name, [H|T], Variables) -> remove(Name, T, [H|Variables]);
remove(_, [], Variables) -> Variables.

get(Name, [{Name, Value} | _]) -> Value;
get(Name, [_|Rest]) -> get(Name, Rest);
get(_, []) -> {error, "This value doesn't exist"}.

merge(Source, Update) -> merge(Source, Update, []).
merge([{Name, Value}|Rest], Update, Result) ->
    case get(Name, Update) of
	{error, _} -> merge(Rest, Update, [{Name, Value}|Result]);
	NewValue -> merge(Rest, Update, [{Name, NewValue}|Result])
    end;
merge([], _, Result) -> Result.
