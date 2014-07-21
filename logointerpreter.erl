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
run_instruction({repeat, {number, _, Times}, Instructions}, Variables) ->
    LoopVariables = add({loop, 1}, Variables),
    case run_instruction({repeat, Times, Instructions}, LoopVariables) of
	{ok, Result} -> {ok, merge(Variables, Result)};
	Error -> Error
    end;
run_instruction({repeat, 0, _}, Variables) -> {ok, Variables};
run_instruction({repeat, Times, Instructions}, Variables) ->
    case run(Instructions, Variables) of
	{ok, NewVariables} ->
	    run_instruction({repeat, Times - 1, Instructions}, increment(loop, NewVariables));
	Error -> Error
    end;
% Basic commands
run_instruction({Symbol, ArgumentList}, Variables) ->
    ComputedArguments = compute_arguments(ArgumentList, Variables),
    Drawer = drawer(),
    Drawer:do(Symbol, ComputedArguments),
    {ok, Variables};
% Default : error
run_instruction(Instruction, _) ->
    io:format("Bad Instruction :  ~p !\n", [Instruction]).

compute_arguments(List, Variables) -> compute_arguments(List, Variables, []).
compute_arguments([H|T], Variables, Result) ->
    Argument = compute_aexpr(H, Variables),
    compute_arguments(T, Variables, [Argument|Result]);
compute_arguments([], _, Result) -> Result.

compute_aexpr({rand, [Expr]}, Variables) ->
    {number, ComputedExpr} = compute_aexpr(Expr, Variables),
    {number, random:uniform(ComputedExpr)};
compute_aexpr({angle}, _) ->
    Drawer = drawer(),
    Drawer:angle();
compute_aexpr({loop}, Variables) ->
    case get(loop, Variables) of
	{ok, Value} -> {number, Value};
	Other ->  Other
    end;
compute_aexpr({number, _, Value}, _) -> {number, Value};
compute_aexpr({Operator, {A, B}}, Variables) ->
    {number, ComputedA} = compute_aexpr(A, Variables),
    {number, ComputedB} = compute_aexpr(B, Variables),
    {number, compute(Operator, ComputedA, ComputedB)}.

compute(plus, A, B) -> A + B;
compute(minus, A, B) -> A - B;
compute(multiply, A, B) -> A * B;
compute(divide, A, B) -> A / B.

drawer() -> logofakedrawer.


% Variables management
add({Name, Value}, Variables) ->
    NewVariables = remove(Name, Variables),
    [{Name, Value} | NewVariables].

remove(Name, Variables) -> remove(Name, Variables, []).
remove(Name, [{Name, _}| Rest], Variables) -> remove(Name, Rest, Variables);
remove(Name, [H|T], Variables) -> remove(Name, T, [H|Variables]);
remove(_, [], Variables) -> Variables.

get(Name, [{Name, Value} | _]) -> {ok, Value};
get(Name, [_|Rest]) -> get(Name, Rest);
get(_, []) -> {error, "This value doesn't exist"}.

merge(Source, Update) -> merge(Source, Update, []).
merge([{Name, Value}|Rest], Update, Result) ->
    case get(Name, Update) of
	{error, _} -> merge(Rest, Update, [{Name, Value}|Result]);
	{ok, NewValue} -> merge(Rest, Update, [{Name, NewValue}|Result])
    end;
merge([], _, Result) -> Result.

increment(Name, Variables) -> increment(Name, Variables, []).
increment(Name, [{Name, Value}|Rest], Result) ->
    increment(Name, Rest, [{Name, Value + 1}|Result]);
increment(Search, [{Name, Value}|Rest], Result) ->
    increment(Search, Rest, [{Name, Value}|Result]);
increment(_, [], Result) -> Result.
