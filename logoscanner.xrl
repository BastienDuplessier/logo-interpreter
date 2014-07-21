Definitions.

NUMBER = [1-9]?[0-9]+(\.[0-9]+)?
SYMBOL = [A-Za-z]+
OPERATOR = [\+\-\*\/]
COMMENT = //.+
REPEAT = REPETE
RANDOM = HASARD
ANGLE = CAP
LOOP = LOOP

Rules.

{COMMENT} : skip_token.
{REPEAT} : {token, {repeat, TokenLine}}.

{NUMBER} : {token, {number, TokenLine, string_to_number(TokenChars)}}.
-{NUMBER} : {token, {number, TokenLine, 0 - string_to_number(TokenChars)}}.
{ANGLE} : {token, {angle, TokenLine}}.
{RANDOM} : {token, {rand, TokenLine}}.
{LOOP} : {token, {loop, TokenLine}}.
{SYMBOL} : {token, string_to_token(TokenChars, TokenLine)}.


\[ : {token, {open_bracket, TokenLine}}.
\] : {token, {close_bracket, TokenLine}}.
\( : {token, {open_parent, TokenLine}}.
\) : {token, {close_parent, TokenLine}}.
{OPERATOR} : {token, {string_to_operator(TokenChars), TokenLine}}.

[\s\n\r] : skip_token.

Erlang code.

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
    string_to_token(String, TokenLine, basic_command(String)).
string_to_token(String, TokenLine, true) ->
    {keyword, TokenLine, convert_keyword(String)};
string_to_token(String, TokenLine, false) ->
    {symbol, TokenLine, String}.

convert_keyword(String) ->
    list_to_atom(string:to_lower(String)).

string_to_operator("+") -> plus;
string_to_operator("-") -> minus;
string_to_operator("*") -> multiply;
string_to_operator("/") -> divide.

basic_command("AV") -> true;
basic_command("TD") -> true;
basic_command("TG") -> true;
basic_command("REC") -> true;
basic_command("FPOS") -> true;
basic_command("FCAP") -> true;
basic_command("VE") -> true;
basic_command("MT") -> true;
basic_command("CT") -> true;
basic_command("LC") -> true;
basic_command("BC") -> true;
basic_command("FCC") -> true;
basic_command(_) -> false.
