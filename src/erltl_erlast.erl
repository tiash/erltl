-module(erltl_erlast).

-export([atom/1]).
-export([binary/1]).
-export([block/1]).
-export([call/2]).
-export(['case'/2]).
-export([clause/3]).
-export([fromTerm/1]).
-export([function/2]).
-export([function/3]).
-export([ilength/1]).
-export([integer/1]).
-export([line/0]).
-export([line/1]).
-export([list/1]).
-export([match/2]).
-export([module/0]).
-export([module/1]).
-export([op/3]).
-export([parse_code/2]).
-export([parse_tokens/2]).
-export([record_new/2]).
-export([record_update/3]).
-export([slength/1]).
-export([string/1]).
-export([term/1]).
-export([tokens/1]).
-export([tuple/1]).
-export([unparse/1]).
-export([unparse_tokens/1]).
-export([untokens/1]).
-export([uvar/0]).
-export([uvar/1]).
-export([var/1]).
-export([bind/2]).
-export([bind/3]).
-export(['try'/2,tryof/3,tryafter/3,tryofafter/4]).

-export([flatten/1]).
-export([is_string/1]).
-export([split_on/2]).

-define(OK(VAL),{ok,VAL}).
-define(ERRINFO(LOC,MOD,INFO),{error,{LOC,MOD,INFO}}).
-define(ERRINFO(LOC,INFO),?ERRINFO(LOC,?MODULE,INFO)).
-define(ERRINFO(INFO),?ERRINFO(line(),INFO)).
-define(ERR(ERR),ERR={error,_}).

-define(FAILCLAUSE(NAME,ARGS),
  io:format("~p:~p("++lists:flatten(intercalate(",",["~p" || _ <- ARGS]))++") -> erlang:error(function_clause).\t\t[~s:~p]~s~n"
             , [?MODULE,NAME|ARGS]++[?FILE,?LINE,stacktrace()]),
  erlang:error(function_clause)).
-define(FAILCLAUSE1(NAME), NAME(A) -> ?FAILCLAUSE(NAME,[A])).
-define(FAILCLAUSE2(NAME), NAME(A,B) -> ?FAILCLAUSE(NAME,[A,B])).
-define(FAILCLAUSE3(NAME), NAME(A,B,C) -> ?FAILCLAUSE(NAME,[A,B,C])).
-define(FAILCLAUSE4(NAME), NAME(A,B,C,D) -> ?FAILCLAUSE(NAME,[A,B,C,D])).



call(FUN={remote,_,_,_},ARGS) -> {call,line(),FUN,ARGS};
call(FUN={atom,_,_},ARGS) -> {call,line(),FUN,ARGS};
call({MOD,FUN,N},ARGS) when N==length(ARGS) -> call({MOD,FUN},ARGS);
call({FUN,N},ARGS) when N==length(ARGS) -> call(FUN,ARGS);
call({MOD,FUN},ARGS) when is_atom(MOD), is_atom(FUN) -> call({remote,line(),atom(MOD),atom(FUN)},ARGS);
call(FUN,ARGS) when is_atom(FUN) -> call(atom(FUN),ARGS);
?FAILCLAUSE2(call).

atom(A) -> {atom,line(),list_to_atom(flat_string(A))}.

var(A) -> {var,line(),list_to_atom(flat_string(A))}.

binary(A={bin,_,_}) -> A;
binary(A) when is_list(A) -> binary_concat(A);
binary({A,binary}) -> {bin,line(),[{bin_element,line(),A,default,[binary]}]};
binary(A) when is_integer(A) -> {bin,line(),[{bin_element,line(),integer(A),default,default}]};
binary(A) when not is_tuple(A) -> {bin,line(),[{bin_element,line(),string(A),default,default}]};
binary(A) when is_tuple(A) -> {bin,line(),[{bin_element,line(),A,default,default}]};
?FAILCLAUSE1(binary).

binary_concat(ELEMS) ->
  case length(ELEMS)>1 andalso lists:all(fun(I)->is_integer(I) end,ELEMS) of
    true -> {bin,line(),[{bin_element,line(),string(ELEMS),default,default}]};
    _ -> {bin,line(),binary_concat(ELEMS,[])}
  end.
binary_concat([],R) -> R;
binary_concat([{bin,_,ELEMS}|C],R) -> binary_concat(C,R++ELEMS);
binary_concat([X|C],R) -> binary_concat([binary(X)|C],R);
?FAILCLAUSE2(binary_concat).

string(A) -> {string,line(),flat_string(A)}.

tuple(A) when is_tuple(A) -> {tuple,line(),tuple_to_list(A)};
?FAILCLAUSE1(tuple).

list([]) -> {nil,line()};
list([H|T]) -> {cons,line(),H,list(T)};
?FAILCLAUSE1(list).

integer(A) when is_integer(A) -> {integer,line(),A};
?FAILCLAUSE1(integer).

flat_string(A) -> lists:reverse(flat_string(A,[])).
flat_string([],R) -> R;
flat_string(A,R) when is_integer(A) -> [A|R];
flat_string(A,R) when is_atom(A) -> lists:reverse(atom_to_list(A)) ++ R;
flat_string(A,R) when is_binary(A) -> lists:reverse(binary_to_list(A)) ++ R;
flat_string([A|T],R) -> flat_string(T,flat_string(A,R));
?FAILCLAUSE2(flat_string).

term(A) when is_atom(A) -> atom(A);
term(A) when is_integer(A) -> integer(A);
term(A) when is_float(A) -> float(A);
term(A) when is_list(A) ->
  case lists:all(fun is_integer/1,A) of
    true -> {string,line(),A};
    false -> list([term(X) || X <- A])
  end;
term(A) when is_binary(A) -> binary(A);
term(A) when is_tuple(A) -> tuple(list_to_tuple([term(X) || X<-tuple_to_list(A)]));
?FAILCLAUSE1(term).

fromTerm({atom,_,A}) -> A;
fromTerm({integer,_,A}) -> A;
fromTerm({float,_,A}) -> A;
fromTerm({string,_,A}) -> A;
fromTerm({cons,_,H,T}) -> [fromTerm(H)|fromTerm(T)];
fromTerm({nil,_}) -> [];
fromTerm({tuple,_,ELEMS}) -> list_to_tuple([fromTerm(X) || X<-ELEMS]);
fromTerm({bin,_,A}) -> iolist_to_binary([fromTerm_binelem(E) || E <- A]);
?FAILCLAUSE1(fromTerm).

fromTerm_binelem({bin_element,_,{string,_,A},default,default}) -> A;
fromTerm_binelem({bin_element,_,{bin,_,A},default,default}) -> [fromTerm_binelem(E) || E <- A];
fromTerm_binelem({bin_element,_,{bin,_,A},default,binary}) -> [fromTerm_binelem(E) || E <- A];
?FAILCLAUSE1(fromTerm_binelem).
  


op(A,OP,B) -> {op,line(),OP,A,B}.

match(LEFT,RIGHT) -> {match,line(),LEFT,RIGHT}.


line() -> 
  case erlang:get('current_line') of
    undefined -> 1;
    L -> L
  end.
line(0) -> erlang:erase('current_line');
line(1) -> erlang:erase('current_line');
line(N) -> erlang:put('current_line',N).

module() -> erlang:get('current_module').
module(undefined) -> erlang:erase('current_module');
module(N) -> erlang:put('current_module',N).

record_new(TAG,VALUES) -> {record,line(),TAG,[record_field(KEY,VALUE) || {KEY,VALUE} <- VALUES]}.
record_update(ORIG,TAG,VALUES) -> {record,line(),ORIG,TAG,[record_field(KEY,VALUE) || {KEY,VALUE} <- VALUES]}.
record_field(KEY,VALUE) when is_atom(KEY) -> record_field(atom(KEY),VALUE);
record_field(KEY,VALUE) -> {record_field,line(),KEY,VALUE}.

uvar() -> uvar('VAR').
uvar(TAG) ->
  N = case erlang:get('var_counter') of
        undefined -> 1;
        A -> A
      end,
  erlang:put('var_counter',N+1),
  var([TAG,integer_to_list(N)]).

function(NAME,CLAUSES) ->
  [{clause,_,P,_,_}|_] = CLAUSES,
  function(NAME,length(P),CLAUSES).
function(NAME,ARITY,CLAUSES) when not is_list(CLAUSES) -> function(NAME,ARITY,[CLAUSES]);
function(NAME,ARITY,CLAUSES) when is_atom(NAME), is_integer(ARITY), is_list(CLAUSES) -> {function,line(),NAME,ARITY,CLAUSES};
function(A,B,C) -> io:format("[~p]: ~p[~p]:function(~p,~p,~p) -> exit(function_clause).~n",[module(),?MODULE,?LINE,A,B,C]).
% clause(PARAMS,BODY) -> clause(PARAMS,[],BODY).
%clause(PARAMS,GUARDS,BODY) when not is_list(PARAMS) -> clause([PARAMS],GUARDS,BODY);
%clause(PARAMS,GUARDS,BODY) when not is_list(GUARDS) -> clause(PARAMS,[GUARDS],BODY);
%clause(PARAMS,GUARDS,BODY) when not is_list(BODY) -> clause(PARAMS,GUARDS,[BODY]);
clause(PARAMS,GUARDS,BODY) -> {clause,line(),PARAMS,GUARDS,BODY}.

'case'(VAL,CLAUSES) -> {'case',line(),VAL,CLAUSES}.
'try'(VAL,ERROR) -> {'try',line(),VAL,[],ERROR,[]}.
tryof(VAL,CLAUSES,ERROR) -> {'try',line(),VAL,CLAUSES,ERROR,[]}.
tryafter(VAL,ERROR, AFTER) -> {'try',line(),VAL,[],ERROR,AFTER}.
tryofafter(VAL,CLAUSES,ERROR,AFTER) -> {'try',line(),VAL,CLAUSES,ERROR,AFTER}.

block([EXPRS]) -> EXPRS;
block(EXPRS) when is_list(EXPRS) -> {block,line(),EXPRS};
block(EXPRS) -> EXPRS.


ilength(VAL) -> length(flat_string(VAL)).
slength(VAL) -> integer(ilength(VAL)).



% Functions to reconstruct source code from the AST

unparse(S) -> untokens(unparse_tokens(S)).

unparse_tokens(S) -> flatten(unparse_tokens_(S)).
unparse_tokens_(A) when is_list(A) -> unparse_tokens_list({',',[]},A);
unparse_tokens_({attribute,LINE,A,B}) -> [{'-',LINE},{atom,LINE,A},{'(',LINE},unparse_tokens_(erl_parse:abstract(B)),{')',LINE},{dot,LINE}];
unparse_tokens_({function,LINE,NAME,_,CLAUSES}) ->
  unparse_tokens_clauses([{atom,LINE,NAME},{'(',LINE}],[{')',LINE}],[{dot,LINE}],CLAUSES);
unparse_tokens_({match,LINE,LEFT,RIGHT}) ->
  [unparse_tokens_(LEFT),{'=',LINE},unparse_tokens_(RIGHT)];
unparse_tokens_({'case',LINE,EXPR,CLAUSES}) ->
  [ {'case',LINE},unparse_tokens_(EXPR),{'of',LINE}
  , unparse_tokens_clauses([],[],[],CLAUSES)
  , {'end',LINE}
  ];
unparse_tokens_({'try',LINE,EXPR,CLAUSES,CATCH,AFTER}) ->
  [ {'try',LINE},unparse_tokens_(EXPR),{'of',LINE}
  , unparse_tokens_clauses([],[],[],CLAUSES)
  , {'catch',LINE}
  , unparse_tokens_clauses([],[],[],CATCH)
  , {'after',LINE}
  , unparse_tokens_(AFTER)
  , {'end',LINE}
  ];
unparse_tokens_({'if',LINE,CLAUSES}) ->
  [ {'if',LINE}
  , unparse_tokens_if(CLAUSES)
  , {'end',LINE}
  ];
unparse_tokens_({block,LINE,CODE}) ->
  [ {'begin',LINE}
  , unparse_tokens_(CODE)
  , {'end',LINE}
  ];
unparse_tokens_({remote,LINE,MOD,FUN}) ->
  [ unparse_tokens_(MOD), {':',LINE}, unparse_tokens_(FUN) ];
unparse_tokens_({call,LINE,FUN,ARGS}) ->
  [ unparse_tokens_(FUN), {'(',LINE}
  , unparse_tokens_list({',',LINE},ARGS)
  , {')',LINE}
  ];
unparse_tokens_({bin,LINE,PARTS}) ->
  [{'<<',LINE},unparse_tokens_(PARTS),{'>>',LINE}];
unparse_tokens_({bin_element,LINE,PART,LEN,KIND}) ->
  [unparse_tokens_(PART)
  , case LEN of
      default -> [];
      LEN -> [{':',LINE},unparse_tokens_(LEN)]
    end
  , case KIND of
      default -> [];
      [] -> [];
      _ -> [{'/',LINE},unparse_tokens_list({'-',LINE},fun
                         ({A,B}) -> [{atom,LINE,A},{':',LINE},{atom,LINE,B}];
                         (A) -> [{atom,LINE,A}]
                       end,KIND)]
    end
  ];
unparse_tokens_(TOK={var,_,_}) -> TOK;
unparse_tokens_(TOK={atom,_,_}) -> TOK;
unparse_tokens_(TOK={integer,_,_}) -> TOK;
unparse_tokens_(TOK={float,_,_}) -> TOK;
unparse_tokens_(TOK={char,_,_}) -> TOK;
unparse_tokens_(TOK={string,_,_}) -> TOK;
unparse_tokens_({tuple,LINE,ELEMS}) ->
  [{'{',LINE},unparse_tokens_list({',',LINE},ELEMS),{'}',LINE}];
unparse_tokens_(TAIL={nil,LINE}) -> unparse_tokens_listcell(LINE,[],TAIL);
unparse_tokens_(TAIL={cons,LINE,_,_}) -> unparse_tokens_listcell(LINE,[],TAIL);
unparse_tokens_({op,LINE,OP,ARG}) -> [{OP,LINE},unparse_tokens_(ARG)];
unparse_tokens_({op,LINE,OP,LEFT,RIGHT}) -> [unparse_tokens_(LEFT),{OP,LINE},unparse_tokens_(RIGHT)];
unparse_tokens_({record,LINE,TAG,FIELDS}) when is_atom(TAG) -> [{'#',LINE},{atom,LINE,TAG},{'{',LINE},unparse_tokens_(FIELDS),{'}',LINE}];
unparse_tokens_({record,LINE,VALUE,TAG,FIELDS}) when is_atom(TAG) -> [unparse_tokens_(VALUE),{'#',LINE},{atom,LINE,TAG},{'{',LINE},unparse_tokens_(FIELDS),{'}',LINE}];
unparse_tokens_({record_index,LINE,TAG,FIELD}) -> [{'#',LINE},{atom,LINE,TAG},{'.',LINE},unparse_tokens_(FIELD)];
unparse_tokens_({record_field,LINE,FIELD,VAL}) -> [{'#',LINE},unparse_tokens_(FIELD),{'.',LINE},unparse_tokens_(VAL)];
unparse_tokens_({record_field,LINE,VALUE,TAG,FIELD}) when is_atom(TAG) -> [unparse_tokens_(VALUE),{'#',LINE},{atom,LINE,TAG},{'.',LINE},unparse_tokens_(FIELD)];
unparse_tokens_({eof,LINE}) -> {comment,LINE,"\n%%% EOF %%%\n;"};
unparse_tokens_({error,E}) -> {comment,0,io_lib:format("\n%% error: ~p\n",[E])};
unparse_tokens_({warning,E}) -> {comment,0,io_lib:format("\n%% warning: ~p\n",[E])};
?FAILCLAUSE1(unparse_tokens_).

unparse_tokens_listcell(LINE,LIST,{cons,_,HEAD,TAIL}) -> unparse_tokens_listcell(LINE,[HEAD|LIST],TAIL);
unparse_tokens_listcell(LINE,LIST,{nil,_}) -> [{'[',LINE},unparse_tokens_list({',',LINE},lists:reverse(LIST)),{']',LINE}];
unparse_tokens_listcell(LINE,LIST,TAIL) -> [{'[',LINE},unparse_tokens_list({',',LINE},lists:reverse(LIST)),{'|',LINE},unparse_tokens_(TAIL),{']',LINE}].

unparse_tokens_list(SEP,LIST) -> unparse_tokens_list(SEP,fun unparse_tokens_/1,LIST).
unparse_tokens_list(_SEP,_FUN,[]) -> [];
unparse_tokens_list(_SEP,FUN,[A]) -> FUN(A);
unparse_tokens_list(SEP,FUN,[A|B]) -> [FUN(A),SEP|unparse_tokens_list(SEP,FUN,B)];
?FAILCLAUSE3(unparse_tokens_list).

unparse_tokens_clauses(_LEFT,_RIGHT,_END,[]) -> [];
unparse_tokens_clauses(LEFT,RIGHT,END,[{clause,LINE,ARGS,GUARDS,CODE}|CLAUSES]) ->
  [ LEFT, case ARGS of
            [] -> [];
            [FST|MORE] -> [unparse_tokens_(FST) | [[{',',LINE},unparse_tokens_(A)] || A <- MORE]]
          end, RIGHT
  , case GUARDS of
      [] -> [];
      [FST|MORE] -> [{'when',LINE},unparse_tokens_(FST) | [[{',',LINE},unparse_tokens_(A)] || A <- MORE]]
    end
  , {'->',LINE}
  , unparse_tokens_(CODE)
  , case CLAUSES of
      [] -> END;
      _ -> {';',LINE}
    end
  | unparse_tokens_clauses(LEFT,RIGHT,END,CLAUSES)];
?FAILCLAUSE4(unparse_tokens_clauses).

unparse_tokens_if([]) -> [];
unparse_tokens_if([{clause,LINE,[],[FST|MORE],CODE}|CLAUSES]) ->
  [ unparse_tokens_(FST) , [[{',',LINE},unparse_tokens_(A)] || A <- MORE]
  , {'->',LINE}
  , unparse_tokens_(CODE)
  , case CLAUSES of
      [] -> [];
      _ -> {';',LINE}
    end
  | unparse_tokens_if(CLAUSES)];
?FAILCLAUSE1(unparse_tokens_if).




untokens(S)  -> iolist_to_binary(untokens_(S)).
untokens_(S) when is_list(S) -> [untokens_(X) || X <- S];
untokens_({dot,_}) -> ".\n";
untokens_({';',_}) -> ";\n";
untokens_({'->',_}) -> " ->\n";
untokens_({'when',_}) -> " when ";
untokens_({'end',_}) -> " end";
untokens_({'if',_}) -> "if ";
untokens_({'case',_}) -> "case ";
untokens_({'try',_}) -> "try ";
untokens_({'receive',_}) -> "receive ";
untokens_({'of',_}) -> " of ";
untokens_({'after',_}) -> " after ";
untokens_({'catch',_}) -> " catch ";
untokens_({'and',_}) -> " and ";
untokens_({'or',_}) -> " or ";
untokens_({'andalso',_}) -> " andalso ";
untokens_({'orelse',_}) -> " orelse ";
untokens_({'<<',_}) -> " <<";
untokens_({'>>',_}) -> ">> ";
untokens_({'[',_}) -> " [";
untokens_({']',_}) -> "] ";
untokens_({'(',_}) -> " (";
untokens_({')',_}) -> ") ";
untokens_({',',_}) -> ", ";
untokens_({'_',_}) -> " _ ";
untokens_({X,_}) -> [" ",atom_to_list(X)," "];
untokens_({atom,_,'when'}) -> "'when'";
untokens_({atom,_,'case'}) -> "'case'";
untokens_({atom,_,'of'}) -> "'of'";
untokens_({atom,_,'end'}) -> "'end'";
untokens_({atom,_,'if'}) -> "'if'";
untokens_({atom,_,'try'}) -> "'try'";
untokens_({atom,_,'catch'}) -> "'catch'";
untokens_({atom,_,'receive'}) -> "'receive'";
untokens_({atom,_,'after'}) -> "'after'";
untokens_({atom,LINE,ATOM}) when is_atom(ATOM) -> untokens_({atom,LINE,atom_to_list(ATOM)});
untokens_({atom,_,ATOM}) when is_list(ATOM) ->
  case lists:all(fun
          (A) when $a =< A, A =< $z -> true;
          (A) when $A =< A, A =< $Z -> true;
          ($_) -> true;
          (_) -> false
        end,ATOM) of
    true -> ATOM;
    false -> [$',ATOM,$']
  end;
untokens_({char,_,CHAR}) -> [$$,CHAR];
untokens_({comment,_,COMMENT}) -> [$%,COMMENT,$\n];
untokens_({float,_,FLOAT}) -> [float_to_list(FLOAT)];
untokens_({integer,_,INT}) -> [integer_to_list(INT)];
untokens_({var,_,'_'}) -> [" _ "];
untokens_({var,_,VAR}) -> [atom_to_list(VAR)];
untokens_({whitespace,_,WS}) -> WS;
untokens_({string,_,STR}) -> [io_lib:format("~p",[STR])];
?FAILCLAUSE1(untokens_).

%parse(KIND,CODE) ->
%  case tokens(CODE,true) of
%    ?OK(TOKS) -> parse_tokens(KIND,TOKS);
%    ?ERR(ERR) -> ERR
%  end.

parse_code(KIND,CODE) ->
  case tokens(CODE) of
    ?OK(TOKS) -> parse_tokens(KIND,TOKS);
    ?ERR(ERR) -> {error,{ERR,in,CODE}}
  end.

tokens(CODE) when is_binary(CODE) -> tokens(binary_to_list(CODE));
tokens(CODE_) -> 
  CODE=flatten(CODE_),
  case erl_scan:string(CODE) of
    {ok,TOK,_} -> ?OK(TOK);
    {error,INFO,_} -> {error,{INFO,in,CODE}}
  end.

parse_tokens(form,TOKENS) -> erl_parse:parse_form(TOKENS);
parse_tokens(term,TOKENS) -> erl_parse:parse_term(TOKENS);
parse_tokens(exprs,TOKENS) ->
  case lists:last(TOKENS) of
    {dot,_} -> erl_parse:parse_exprs(TOKENS);
    % _       -> erl_parse:parse_exprs(exprs,TOKENS ++ [{dot,0}]) of
    _ -> case erl_parse:parse_exprs(TOKENS) of
           RES when element(1,RES)==error ->
             case erl_parse:parse_exprs(TOKENS ++ [{dot,0}]) of
                NRES when element(1,NRES)==ok -> NRES;
                _ -> RES
             end;
           RES -> RES
         end
  end;
parse_tokens(forms,TOKENS) ->
  FORMS = [parse_tokens(form,T++[{dot,-1}]) || T <- split_on(fun ({dot,_})->true; (_)->false end,TOKENS), T=/=[]],
  case lists:all(fun (?OK(_)) -> true; (_) -> false end,FORMS) of
    true -> ?OK([F || ?OK(F) <- FORMS]);
    false -> ERRS = [ E || ?ERR(E) <- FORMS ], {error,{ERRS,in,TOKENS}}
  end;
parse_tokens(args,[]) -> ?OK([]);
parse_tokens(args,TOKENS) -> parse_tokens(exprs,TOKENS);
parse_tokens(guards,TOKENS) -> parse_tokens(exprs,TOKENS);
parse_tokens(_,_) -> ?ERRINFO(unkonwn_form).



-define(BB(T),bind(__BIND__,T)).
?BB({attribute,LINE,KEY,VAL}) -> {attribute,LINE,KEY,VAL};
?BB({function,LINE,NAME,ARITY,CLAUSES}) -> {function,LINE,NAME,ARITY,?BB(CLAUSES)};
?BB({record_field,LINE,TERM}) -> {record_field,LINE,?BB(TERM)};
?BB({record_field,LINE,KEY,VAL}) -> {record_field,LINE,?BB(KEY),?BB(VAL)};
?BB({error,E}) -> {error,E};
?BB({warning,E}) -> {warning,E};
?BB({eof,LINE}) -> {eof,LINE};
?BB(VAL = {integer,_,_}) -> VAL;
?BB(VAL = {float,_,_}) -> VAL;
?BB(VAL = {string,_,_}) -> VAL;
?BB(VAL = {atom,_,_}) -> VAL;
?BB({match,LINE,LEFT,RIGHT}) -> {match,LINE,?BB(LEFT),?BB(RIGHT)};
?BB({tuple,LINE,TERMS}) -> {tuple,LINE,?BB(TERMS)};
?BB({nil,LINE}) -> {nil,LINE};
?BB({cons,LINE,HEAD,TAIL}) -> {cons,LINE,?BB(HEAD),?BB(TAIL)};
?BB({bin,LINE,TERMS}) -> {bin,LINE,?BB(TERMS)};
?BB({bin_element,LINE,VAL,SIZE,TYPE}) -> {bin_element,LINE,?BB(VAL),?BB(SIZE),TYPE};
?BB(default) -> default;
?BB({op,LINE,OP,LEFT,RIGHT}) -> {op,LINE,OP,?BB(LEFT),?BB(RIGHT)};
?BB({op,LINE,OP,TERM}) -> {op,LINE,OP,?BB(TERM)};
?BB({record,LINE,NAME,TERMS}) -> {record,LINE,NAME,?BB(TERMS)};
% ?BB({record_field,LINE,KEY,VAL}) -> {record_field,LINE,?BB(KEY),?BB(VAL)};
?BB({record,LINE,VAL,NAME,TERMS}) -> {record,LINE,?BB(VAL),NAME,?BB(TERMS)};
?BB({record_index,LINE,NAME,FIELD}) -> {record_index,LINE,NAME,?BB(FIELD)};
?BB({record_index,LINE,VAL,NAME,FIELD}) -> {record_index,LINE,?BB(VAL),NAME,?BB(FIELD)};
?BB({'catch',LINE,TERM}) -> {'catch',LINE,?BB(TERM)};
?BB({call,LINE,FUN,ARGS}) -> {call,LINE,?BB(FUN),?BB(ARGS)};
?BB({remote,LINE,MOD,FUN}) -> {remote,LINE,?BB(MOD),?BB(FUN)};
?BB({lc,LINE,TERM,GEN}) -> {lc,LINE,?BB(TERM),?BB(GEN)};
?BB({bc,LINE,TERM,GEN}) -> {bc,LINE,?BB(TERM),?BB(GEN)};
?BB({block,LINE,TERM}) -> {block,LINE,?BB(TERM)};
?BB({'if',LINE,CLAUSES}) -> {'if',LINE,?BB(CLAUSES)};
?BB({'case',LINE,VAR,CLAUSES}) -> {'case',LINE,?BB(VAR),?BB(CLAUSES)};
?BB({'try',LINE,VAL,CASE,CATCH,FINALY}) -> {'try',?BB(LINE),?BB(VAL),?BB(CASE),?BB(CATCH),?BB(FINALY)};
?BB({'receive',LINE,CASE}) -> {'receive',LINE,?BB(CASE)};
?BB({'receive',LINE,CASE,AFTER,ELSE}) -> {'receive',LINE,?BB(CASE),?BB(AFTER),?BB(ELSE)};
?BB({'fun',LINE,FUN}) -> {'fun',LINE,?BB(FUN)};
?BB(FUN={function,_,_}) -> FUN;
?BB(FUN={function,_,_,_}) -> FUN;
?BB({clauses,CLAUSES}) -> {clauses,?BB(CLAUSES)};
?BB({'query',LINE,LC}) -> {'query',LINE,?BB(LC)};
?BB({generate,LINE,VAL,SRC}) -> {generate,LINE,?BB(VAL),?BB(SRC)};
?BB({b_generate,LINE,VAL,SRC}) -> {b_generate,LINE,?BB(VAL),?BB(SRC)};
?BB({clause,LINE,PATERN,GUARD,ACTION}) -> {clause,LINE,?BB(PATERN),?BB(GUARD),?BB(ACTION)};
?BB(TERM) when is_list(TERM) -> [?BB(T) || T<-TERM];
bind(A,TERM) when is_list(A) -> lists:foldl(fun bind/2,TERM,A);
bind({VAR,VAL},TERM={var,_,VAR2}) ->
  if VAR==VAR2 -> VAL;
     true -> TERM
  end;
?FAILCLAUSE2(bind).


bind(K,V,TERM) -> bind({K,V},TERM).






stacktrace() -> 
  {'EXIT',{undef,STACK}} = (catch no_module:no_fun()),
  stacktrace(tl(tl(tl(STACK)))).
stacktrace(STACK) when is_list(STACK) -> [stacktrace(FRAME) || FRAME <- STACK];
stacktrace({MOD,FUN,ARITY}) -> io_lib:format("\n  called at ~p:~p/~p",[MOD,FUN,ARITY]);
?FAILCLAUSE1(stacktrace).

intercalate(_,[]) -> [];
intercalate(_,[ITEM]) -> [ITEM];
intercalate(SEP,[HEAD|TAIL]) -> [HEAD,SEP|intercalate(SEP,TAIL)];
?FAILCLAUSE2(intercalate).


flatten(A) -> lists:reverse(flatten(A,[])).
flatten([A|B],ACCUM) -> flatten(B,flatten(A,ACCUM));
flatten([],ACCUM) -> ACCUM;
flatten(A,ACCUM) -> [A|ACCUM].

is_string([]) -> true;
is_string([H|T]) when is_integer(H) -> is_string(T);
is_string(_) -> false.

split_on(FUN,LIST) -> split_on(FUN,LIST,[],[]).
split_on(_FUN,[],[],ACCUM) -> lists:reverse(ACCUM);
split_on(FUN,[],X,ACCUM) -> split_on(FUN,[],[],[X|ACCUM]);
split_on(FUN,[H|T],X,ACCUM) ->
  case FUN(H) of
    true -> split_on(FUN,T,[],[lists:reverse(X)|ACCUM]);
    false -> split_on(FUN,T,[H|X],ACCUM)
  end.

