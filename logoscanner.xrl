Definitions.

INT = [1-9]?[0-9]+
SYMBOL = [A-Za-z]+
OPERATOR = [\+\-\*\\]

Rules.

{INT} : {token, {int, string_to_int(TokenChars)}}.
-{INT} : {token, {int, 0 - string_to_int(TokenChars)}}.
{SYMBOL} : {token, string_to_token(TokenChars)}.

\[ : {token, {open_bracket}}.
\] : {token, {close_bracket}}.
{OPERATOR} : {token, {string_to_operator(TokenChars)}}.
[\s\n\r] : skip_token.

Erlang code.

string_to_int(String) ->
    string_to_int(String, 0).

string_to_int([H|T], Value) ->
    NewValue = (Value * 10) + char_to_int(H),
    string_to_int(T, NewValue);
string_to_int([], Value) -> Value.

char_to_int(Char) when Char >= $0, Char =< $9 ->
    Char - $0;
char_to_int(_) -> 0.

string_to_token(String) ->
    string_to_token(String, basic_command(String)).
string_to_token(String, true) ->
    {keyword, convert_keyword(String)};
string_to_token(String, false) ->
    {symbol, String}.

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
