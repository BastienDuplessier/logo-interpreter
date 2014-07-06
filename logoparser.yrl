Nonterminals instructions instruction arguments argument_list.
Terminals keyword int open_bracket close_bracket.
Rootsymbol instructions.

instructions -> instruction instructions : ['$1'|'$2'].
instructions -> '$empty' : [].

instruction -> keyword arguments : {value_of('$1'), '$2'}.

arguments -> '$empty' : [].
arguments -> int : ['$1'].
arguments -> open_bracket int int argument_list close_bracket : ['$2','$3'|'$4'].

argument_list -> '$empty' : [].
argument_list -> int argument_list : ['$1'|'$2'].

Erlang code.
value_of({_, Value}) -> Value.
