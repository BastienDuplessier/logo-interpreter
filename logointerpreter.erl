-module(logointerpreter).
-export([execute/1, file/1]).

file(Filename) ->
    {ok, File} = file:read_file(Filename),
    case unicode:characters_to_list(File) of
	{_, _, _} -> io:format("Your file is crap, go fuck yourself");
	Result -> execute(Result)
    end.

execute(String) ->
    {ok, Tokens, _} = logoscanner:string(String),
    {ok, ParseTree} = logoparser:parse(Tokens),
    {ok, Functions, Instructions} = prepare(ParseTree),
    run(Instructions, [], Functions).

prepare({Functions, Instructions}) ->
    {ok, Functions, Instructions}.

run([Instruction|Rest], Variables, Functions) ->
    case run_instruction(Instruction, Variables, Functions) of
	{ok, NewVariables} -> run(Rest, NewVariables, Functions);
	{error, Error} ->
	    io:format("There was an error on ~p\n", [Instruction]),
	    {error, Error}
    end;
run(_, Variables, _) -> {ok, Variables}.


% Repeat
run_instruction({repeat, {number, _, Times}, Instructions}, Variables, Functions) ->
    LoopVariables = add({loop, 1}, Variables),
    case run_instruction({repeat, Times, Instructions}, LoopVariables, Functions) of
	{ok, Result} -> {ok, merge(Variables, Result)};
	Error -> Error
    end;
run_instruction({repeat, 0, _}, Variables, _) -> {ok, Variables};
run_instruction({repeat, Times, Instructions}, Variables, Functions) ->
    case run(Instructions, Variables, Functions) of
	{ok, NewVariables} ->
	    run_instruction({repeat, Times - 1, Instructions}, increment(loop, NewVariables), Functions);
	Error -> Error
    end;
% While
run_instruction({while, StopExpr, Instructions}, Variables, Functions) ->
    case compute_expr(StopExpr, Variables, Functions) of
	{boolean, true} ->
	    {ok, NewVariables} = run(Instructions, Variables, Functions),
	    run_instruction({while, StopExpr, Instructions}, NewVariables, Functions);
	{boolean, false} -> {ok, Variables};
	Error -> Error
    end;
% If
run_instruction({'if', BoolExpr, IfTrue, IfFalse}, Variables, Functions) ->
    case compute_expr(BoolExpr, Variables, Functions) of
	{boolean, true} -> run(IfTrue, Variables, Functions);
	{boolean, false} -> run(IfFalse, Variables, Functions)
    end;
% Variable management
run_instruction({set, {Name, RawValue}}, Variables, Functions) ->
    Value = compute_expr(RawValue, Variables, Functions),
    {ok, add({Name, Value}, Variables)};
% Basic commands
run_instruction({Symbol, ArgumentList}, Variables, Functions) ->
    ComputedArguments = compute_arguments(ArgumentList, Variables, Functions),
    Drawer = drawer(),
    Drawer:do(Symbol, ComputedArguments),
    {ok, Variables};
% Default : error
run_instruction(Instruction, _, _) ->
    io:format("Bad Instruction :  ~p !\n", [Instruction]).

compute_arguments(List, Variables, Functions) -> compute_arguments(List, Variables, Functions, []).
compute_arguments([H|T], Variables, Functions, Result) ->
    Argument = compute_expr(H, Variables, Functions),
    compute_arguments(T, Variables, Functions, [Argument|Result]);
compute_arguments([], _, _, Result) -> Result.

compute_expr({rand, [Expr]}, Variables, Functions) ->
    {number, ComputedExpr} = compute_expr(Expr, Variables, Functions),
    {number, random:uniform(ComputedExpr)};
compute_expr({{a_op, _, Op}, {A, B}}, Variables, Functions) ->
    compute_expr({Op, {A, B}}, Variables, Functions);
compute_expr({angle}, _, _) ->
    Drawer = drawer(),
    {number, Drawer:angle()};
compute_expr({pi}, _, _) ->
    {number, math:pi()};
compute_expr({loop}, Variables, _) ->
    case get(loop, Variables) of
	{ok, Value} -> Value;
	Other ->  Other
    end;
compute_expr({fact, Value}, Variables, Functions) ->
    {number, N} = compute_expr(Value, Variables, Functions),
    lists:foldl(fun(X, F) -> X * F end, 1, lists:seq(1, trunc(N)));
compute_expr({get, Name}, Variables, _) ->
    case get(Name, Variables) of
	{ok, Value} -> Value;
	Error -> Error
    end;
compute_expr({{a_func, _, Op}, RawValue}, Variables, Functions) ->
    {number, Value} = compute_expr(RawValue, Variables, Functions),
    compute(Op, {Value});
compute_expr({number, _, Value}, _, _) -> {number, Value};
compute_expr({Operator, {A, B}}, Variables, Functions) ->
    {_, ComputedA} = compute_expr(A, Variables, Functions),
    {_, ComputedB} = compute_expr(B, Variables, Functions),
    compute(Operator, {ComputedA, ComputedB}).

compute(plus, {A, B}) -> {number, A + B};
compute(minus, {A, B}) -> {number, A - B};
compute(multiply, {A, B}) -> {number, A * B};
compute(divide, {A, B}) -> {number, A / B};
compute('div', {A, B}) -> {number, A div B};
compute(mod, {A, B}) -> {number, A rem B};
compute("=", {A, B}) -> {boolean, A == B};
compute(">=", {A, B}) -> {boolean, A >= B};
compute("<=", {A, B}) -> {boolean, A =< B};
compute("<>", {A, B}) -> {boolean, A /= B};
compute(">", {A, B}) -> {boolean, A > B};
compute("<", {A, B}) -> {boolean, A < B};
compute("SQRT", {A}) -> {number, math:sqrt(A)};
compute("SIN", {A}) -> {number, math:sin(A)};
compute("COS", {A}) -> {number, math:cos(A)};
compute("EXP", {A}) -> {number, math:exp(A)};
compute("LOG", {A}) -> {number, math:log(A)};
compute("ABS", {A}) when A >= 0 -> {number, A};
compute("ABS", {A}) -> {number, -A};
compute("TAN", {A}) -> {number, math:tan(A)};
compute("&", {A, B}) -> {boolean, A and B};
compute("|", {A, B}) -> {boolean, A or B}.

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
