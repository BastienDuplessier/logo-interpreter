Nonterminals instructions instruction arguments.
Terminals keyword int.
Rootsymbol instructions.

instructions -> instruction instructions : ['$1'|'$2'].
instructions -> '$empty' : [].

instruction -> keyword arguments : {value_of('$1'), '$2'}.

arguments -> int arguments : ['$1' | '$2'].
arguments -> '$empty' : [].

Erlang code.

value_of({_, Value}) -> Value.
