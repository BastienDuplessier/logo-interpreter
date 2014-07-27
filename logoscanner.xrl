Definitions.

NUMBER = [1-9]?[0-9]+(\.[0-9]+)?
SYMBOL = [A-Za-z]+
OPERATOR = [\+\-\*\/!]
COMMENT = //.+

Rules.

{COMMENT} : skip_token.

<> : {token, {b_op, TokenLine, TokenChars}}.
>\= : {token, {b_op, TokenLine, TokenChars}}.
<\= : {token, {b_op, TokenLine, TokenChars}}.
> : {token, {b_op, TokenLine, TokenChars}}.
< : {token, {b_op, TokenLine, TokenChars}}.
\= :  {token, {b_op, TokenLine, TokenChars}}.

\: : {token, {get, TokenLine}}.
\" : {token, {set, TokenLine}}.

DIV : {token, {a_op, TokenLine, 'div'}}.
MOD : {token, {a_op, TokenLine, mod}}.

SQRT : {token, {a_func, TokenLine, TokenChars}}.
SIN : {token, {a_func, TokenLine, TokenChars}}.
COS : {token, {a_func, TokenLine, TokenChars}}.
EXP : {token, {a_func, TokenLine, TokenChars}}.
LOG : {token, {a_func, TokenLine, TokenChars}}.
ABS : {token, {a_func, TokenLine, TokenChars}}.
TAN : {token, {a_func, TokenLine, TokenChars}}.
PI : {token, {pi, TokenLine}}.

{NUMBER} : {token, {number, TokenLine, string_to_number(TokenChars)}}.
-{NUMBER} : {token, {number, TokenLine, 0 - string_to_number(TokenChars)}}.
{SYMBOL} : {token, string_to_token(TokenChars, TokenLine)}.

\[ : {token, {open_bracket, TokenLine}}.
\] : {token, {close_bracket, TokenLine}}.
\( : {token, {open_parent, TokenLine}}.
\) : {token, {close_parent, TokenLine}}.
{OPERATOR} : {token, {string_to_operator(TokenChars), TokenLine}}.

[\s\n\r] : skip_token.

Erlang code.

basic_commands() ->
    ["AV", "TD", "TG", "REC", "FPOS", "FCAP", "VE", "MT", "CT", "LC", "BC", "FCC", "ECRIS"].
reserved_words() ->
    ["REPETE", "HASARD", "CAP", "LOOP", "SI", "DONNE", "TANTQUE"].

string_to_number(String) ->
    string_to_number(String, 0).

string_to_number([$.|T], Value) ->
    Value + string_to_decimals(T);
string_to_number([H|T], Value) ->
    NewValue = (Value * 10) + char_to_int(H),
    string_to_number(T, NewValue);
string_to_number([], Value) -> Value.

string_to_decimals(String) ->
    string_to_decimals(lists:reverse(String), 0).
string_to_decimals([H|T], Value) ->
    NewValue = Value * 0.1 + char_to_int(H),
    string_to_decimals(T, NewValue);
string_to_decimals([], Value) -> Value * 0.1.


char_to_int(Char) when Char >= $0, Char =< $9 ->
    Char - $0;
char_to_int(_) -> 0.

string_to_token(String, TokenLine) ->
    case {basic_command(String), reserved_word(String)} of
	{true, _} -> {command, TokenLine, convert_command(String)};
	{_, true} -> {convert_command(String), TokenLine};
	_ -> {symbol, TokenLine, String}
    end.

convert_command(String) ->
    list_to_atom(string:to_lower(String)).

basic_command(String) ->
    lists:member(String, basic_commands()).
reserved_word(String) ->
    lists:member(String, reserved_words()).

string_to_operator("+") -> plus;
string_to_operator("-") -> minus;
string_to_operator("*") -> multiply;
string_to_operator("/") -> divide;
string_to_operator("!") -> fact.

