Nonterminals program functions function params instructions instruction arguments argument_list a_expr b_expr instructions_list.
Terminals command number open_bracket close_bracket plus minus multiply divide open_parent close_parent repete hasard cap loop b_op si donne set symbol get tantque a_op pi a_func fact b_comp pour fin ret.

Rootsymbol program.

Left 100 b_comp.
Left 150 b_op.
Left 200 plus minus.
Left 300 multiply divide.
Left 400 a_op.
Unary 500 fact.

program -> functions instructions : {'$1', '$2'}.

functions -> function functions : ['$1'|'$2'].
functions -> '$empty' : [].

function -> pour symbol params instructions fin : {func, '$2', '$3', '$4'}.

params -> get symbol params : ['$2'|'$3'].
params -> '$empty' : [].

instructions -> instruction instructions : ['$1'|'$2'].
instructions -> '$empty' : [].

instruction -> command arguments : {value_of('$1'), '$2'}.
instruction -> repete a_expr instructions_list : {repeat, '$2', '$3'}.
instruction -> si b_expr instructions_list instructions_list : {'if', '$2', '$3', '$4'}.
instruction -> si b_expr instructions_list : {'if', '$2', '$3', []}.
instruction -> donne set symbol a_expr : {set, {value_of('$3'), '$4'}}.
instruction -> tantque b_expr instructions_list : {while, '$2', '$3'}.
instruction -> ret a_expr : {ret, '$2'}.

instructions_list -> open_bracket instructions close_bracket : '$2'.


arguments -> '$empty' : [].
arguments -> a_expr : ['$1'].
arguments -> open_bracket a_expr a_expr argument_list close_bracket : ['$2','$3'|'$4'].

argument_list -> '$empty' : [].
argument_list -> a_expr argument_list : ['$1'|'$2'].


a_expr -> a_expr plus a_expr : {symbol_of('$2'), {'$1', '$3'}}.
a_expr -> a_expr minus a_expr : {symbol_of('$2'), {'$1', '$3'}}.
a_expr -> a_expr multiply a_expr : {symbol_of('$2'), {'$1', '$3'}}.
a_expr -> a_expr divide a_expr : {symbol_of('$2'), {'$1', '$3'}}.
a_expr -> a_expr a_op a_expr : {'$2', {'$1', '$3'}}.
a_expr -> a_expr fact : {fact, '$1'}.
a_expr -> number : '$1'.
a_expr -> pi : {pi}.	       
a_expr -> cap : {angle}.
a_expr -> hasard a_expr : {rand, ['$2']}.
a_expr -> loop : {loop}.
a_expr -> a_func a_expr : {'$1', '$2'}.
a_expr -> get symbol : {get, value_of('$2')}.
a_expr -> open_parent a_expr close_parent : '$2'.

b_expr -> a_expr b_op a_expr : {value_of('$2'), {'$1', '$3'}}.
b_expr -> b_expr b_comp b_expr : {value_of('$2'), {'$1', '$3'}}.
b_expr -> open_parent b_expr close_parent : '$2'.

Erlang code.
value_of({_, _, Value}) -> Value.
symbol_of({Symbol, _}) -> Symbol.
