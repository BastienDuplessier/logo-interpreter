Nonterminals instructions instruction arguments argument_list a_expr.
Terminals command number open_bracket close_bracket plus minus multiply divide open_parent close_parent repete hasard cap loop.
Rootsymbol instructions.
Left 200 plus minus.
Left 300 multiply divide.

instructions -> instruction instructions : ['$1'|'$2'].
instructions -> '$empty' : [].

instruction -> command arguments : {value_of('$1'), '$2'}.
instruction -> repete a_expr open_bracket instructions close_bracket : {repeat, '$2', '$4'}.

arguments -> '$empty' : [].
arguments -> a_expr : ['$1'].
arguments -> open_bracket a_expr a_expr argument_list close_bracket : ['$2','$3'|'$4'].

argument_list -> '$empty' : [].
argument_list -> a_expr argument_list : ['$1'|'$2'].

a_expr -> a_expr plus a_expr : {symbol_of('$2'), {'$1', '$3'}}.
a_expr -> a_expr minus a_expr : {symbol_of('$2'), {'$1', '$3'}}.
a_expr -> a_expr multiply a_expr : {symbol_of('$2'), {'$1', '$3'}}.
a_expr -> a_expr divide a_expr : {symbol_of('$2'), {'$1', '$3'}}.
a_expr -> number : '$1'.
a_expr -> cap : {angle}.
a_expr -> hasard a_expr : {rand, ['$2']}.
a_expr -> loop : {loop}.
a_expr -> open_parent a_expr close_parent : '$2'.

Erlang code.
value_of({_, _, Value}) -> Value.
symbol_of({Symbol, _}) -> Symbol.
