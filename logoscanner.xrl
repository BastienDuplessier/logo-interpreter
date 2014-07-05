Definitions.

INT = [1-9]*[0-9]
SYMBOL = [A-Za-z]+

Rules.

{INT} : {token, {int, string_to_int(TokenChars)}}.
-{INT} : {token, {int, 0 - string_to_int(TokenChars)}}.
{SYMBOL} : {token, string_to_token(TokenChars)}.
\s : skip_token.

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
    string_to_token(String, reserved_word(String)).
string_to_token(String, true) ->
    {keyword, String};
string_to_token(String, false) ->
    {symbol, String}.

reserved_word("AV") -> true;
reserved_word("TD") -> true;
reserved_word("TG") -> true;
reserved_word("REC") -> true;
reserved_word("FPOS") -> true;
reserved_word("FCAP") -> true;
reserved_word("VE") -> true;
reserved_word("MT") -> true;
reserved_word("CT") -> true;
reserved_word("LC") -> true;
reserved_word("BC") -> true;
reserved_word("FCC") -> true;
reserved_word(_) -> false.
