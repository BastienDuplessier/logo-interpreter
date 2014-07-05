Definitions.

INT = [1-9]*[0-9]

Rules.

{INT} : {token, {int, string_to_int(TokenChars)}}.
-{INT} : {token, {int, 0 - string_to_int(TokenChars)}}.

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
