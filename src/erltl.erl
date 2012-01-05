-module(erltl).

-export([compile/3]).
% -export([parse_fragments/2]).
% -export([to_forms/2]).

-import(erltl_erlast,
  [ call/2
  , atom/1
  , var/1
  , binary/1
  , string/1
  , tuple/1
  , list/1
  , integer/1
  , term/1
  , op/3
  , line/0
  , line/1
  , match/2
  , record_new/2
  , record_update/3
  , module/1
  , module/0
  , uvar/1
  , uvar/0
  , slength/1
  , ilength/1
  , function/3
  , function/2
  , clause/3
  , clause/2
  , 'case'/2
  , block/1
  % , unparse/1
  % , parse/2
  , parse_code/2
  , parse_tokens/2
  , untokens/1
  , tokens/1
  ]).

-define(OK(VAL),{ok,VAL}).
-define(ERRINFO(LOC,MOD,INFO),{error,{LOC,MOD,INFO}}).
-define(ERRINFO(LOC,INFO),?ERRINFO(LOC,?MODULE,INFO)).
-define(ERRINFO(INFO),?ERRINFO(line(),INFO)).
-define(ERR(ERR),ERR={error,_}).

-define(PART(STR,START),?PART((STR),(START),byte_size(STR))).
-define(PART(STR,START,END),binary:part((STR),(START),(END)-(START))).
-define(STRIP(STR),iolist_to_binary(string:strip(binary_to_list(STR)))).
-define(ATOM(STR),(fun (SS)-> case token(SS) of ?OK([{atom,_,A}])->?OK(A); ?OK(_)->?ERRINFO(not_atom); ?ERR(ERR)->ERR end end(STR))).
-define(MATCH(STR,PTRN),binary:match((STR),<<PTRN>>)).
-define(MATCH(STR,START,PTRN),?MATCH(STR,(START),byte_size(STR),PTRN)).
-define(MATCH(STR,START,END,PTRN),binary:match((STR),<<PTRN>>,[{scope,{(START),(END)-(START)}}])).

-define(CONF(ATR),config(CONFIG,ATR)).

compile(FILE,MODULE,CONFIG) ->
  % io:format("compile(~p,~p)...",[FILE,CONFIG]),
    % io:format("ERLTL: compiling: ~s~n",[FILE]),
    RES = (catch compile_forms(CONFIG,preprocess(CONFIG,sanitize_forms(flatten([{attribute,1,file,{FILE,1}},{attribute,1,module,MODULE},{attribute,1,compile,export_all},to_forms(CONFIG,normalise_fragments(parse_fragments(file:read_file(FILE))))]))))),
    % io:format("ERLTL: done~n  ~p~n",[RES]),
    % io:format("compile(~p,~p) -> ~p~n",[FILE,CONFIG,RES]),
    RES.
  % catch
    % ERROR -> io:format("ERROR in ~p[~p]:compile(...) -> throw(~p).",[?MODULE,?LINE,ERROR]), throw(ERROR)
    % exit:ERROR -> io:format("compile(...) -> ..., exit(~p).~n",[ERROR]), erlang:exit(ERROR);
    %error:ERROR -> io:format("compile(...) -> ..., error(~p).~n",[ERROR]), erlang:error(ERROR);
    % throw:ERROR -> io:format("compile(...) -> ..., throw(~p).~n",[ERROR]), erlang:throw(ERROR)
  % end.

% concat(CODE) -> concat(CODE,[]).
% concat(?OK(CODE),ACCUM) -> concat(CODE,ACCUM);
% concat(?ERR(ERR),_ACCUM) -> ERR;
% concat(CODE,?OK(ACCUM)) -> concat(CODE,ACCUM);
% concat(_CODE, ?ERR(ERR)) -> ERR;
% concat([?OK(CODE)|TAIL],ACCUM) -> concat([CODE|TAIL],ACCUM);
% concat([?ERR(ERR)|_],_ACCUM) -> ERR;
% concat([CODE|TAIL],ACCUM) -> concat(TAIL,ACCUM++CODE);
% concat([],ACCUM) -> ?OK(ACCUM).

flatten(CODE) -> 
  %io:format("~p:flatten(~p)",[?MODULE,CODE]),
  RES = flatten(CODE,[]),
  %io:format(" -> ~p~n",[RES]),
  RES.
flatten(?OK(CODE),ACCUM) -> flatten(CODE,ACCUM);
flatten(?ERR(ERR),_ACCUM) -> ERR;
flatten(CODE,?OK(ACCUM)) -> flatten(CODE,ACCUM);
flatten(_CODE, ?ERR(ERR)) -> ERR;
flatten([?OK(CODE)|TAIL],ACCUM) -> flatten([CODE|TAIL],ACCUM);
flatten([?ERR(ERR)|_],_ACCUM) -> ERR;
flatten([CODE|TAIL],ACCUM) -> flatten(TAIL,flatten(CODE,ACCUM));
flatten([],ACCUM) -> ?OK(ACCUM);
flatten(A,ACCUM) -> flatten([],ACCUM++[A]).


sanitize_forms(?OK(CODE)) -> sanitize_forms(CODE);
sanitize_forms(?ERR(ERR)) -> ERR;
sanitize_forms(CODE) ->
  ?OK(CODE). % TODO!


preprocess(CONFIG,?OK(CODE)) -> preprocess(CONFIG,CODE);
preprocess(_CONFIG,?ERR(ERR)) -> ERR;
preprocess(_CONFIG,CODE) -> CODE. % TODO!

compile_forms(CONFIG,?OK(CODE)) -> compile_forms(CONFIG,CODE);
compile_forms(_CONFIG,?ERR(ERR)) -> ERR;
compile_forms(CONFIG,CODE) ->
  % io:format("ERLTL Code:~n~s~n~n~n",[[erl_pp:form(C)||C<-CODE]]),
  compile:forms(CODE,[binary,return]++CONFIG).
      
      

parse_fragments(?OK(CODE)) -> parse_fragments(CODE);
parse_fragments(?ERR(ERR)) -> ERR;
parse_fragments(CODE) when not is_binary(CODE) -> parse_fragments(iolist_to_binary([CODE]));
parse_fragments(CODE) -> parse_fragments(CODE,[]).
  

parse_fragments(<<>>,ACCUM) -> flatten([lists:reverse(ACCUM)]);
parse_fragments(<<"<%",CODE/binary>>,ACCUM) ->
  case ?MATCH(CODE,"%>") of
    {END,2} -> parse_fragments(?PART(CODE,END+2),[parse_frag(?PART(CODE,0,END))|ACCUM]);
    nomatch -> ?ERRINFO("Unclosed Template escape")
  end;
parse_fragments(CODE,ACCUM) ->
  case ?MATCH(CODE,"<%") of
    {END,2} -> parse_fragments(?PART(CODE,END),[?PART(CODE,0,END)|ACCUM]);
    nomatch -> parse_fragments(<<>>,[CODE|ACCUM])
  end.

normalise_fragments(?OK(A)) -> normalise_fragments(A);
normalise_fragments(?ERR(E)) -> E;
normalise_fragments(A) -> ?OK(normalise_fragments_(A)).
normalise_fragments_([]) -> []; 
normalise_fragments_([<<>>,B])
   -> normalise_fragments_(B);
normalise_fragments_([{expr,[]},B])
   -> normalise_fragments_(B);
normalise_fragments_([A,B|C])
 when is_binary(A), is_binary(B)
   -> normalise_fragments_([<<A/bytes,B/bytes>>|C]);
normalise_fragments_([{expr,A},{expr,B}|C])
   -> normalise_fragments_([{expr,A++B}|C]);
normalise_fragments_([A,<<>>|B])
   -> normalise_fragments_([A|B]);
normalise_fragments_([A,{expr,[]}|B])
   -> normalise_fragments_([A|B]);
normalise_fragments_([A|B])
 when (element(1,hd(B))==param)
   or (element(1,hd(B))==init)
   or (element(1,hd(B))==render)
   -> case is_ws(A) of
        true -> normalise_fragments_(B);
        false -> [A|normalise_fragments_(B)] 
      end;
normalise_fragments_([A|B])
   -> [A|normalise_fragments_(B)].



is_ws(A) when is_binary(A) -> is_ws(binary_to_list(A));
is_ws(A) when is_list(A) -> lists:all(fun is_ws/1,A);
is_ws($ ) -> true;
is_ws($\t) -> true;
is_ws($\n) -> true;
is_ws($\r) -> true;
is_ws(_) -> false.


to_forms(CONFIG,?OK(CODE)) -> to_forms(CONFIG,CODE);
to_forms(_CONFIG,?ERR(ERR)) -> ERR;
to_forms(CONFIG,FRAGS1) ->
  % io:format("Headers...~n",[]),
  {TOPFORMS,FRAGS2} = lists:splitwith(fun ({form,_})->true; (_)->false end,FRAGS1),
  {TOPPARS1,FRAGS3} = lists:splitwith(fun ({param,_})->true; (_)->false end,FRAGS2),
  {TOPINIT1,FRAGS4} = lists:splitwith(fun ({init,_})->true; ({param,_,_})->true; (_)->false end,FRAGS3),
  {TOPEXPRS,FRAGS5} = lists:splitwith(fun ({expr,_})->true; (E)->is_binary(E) end,FRAGS4),
  TOP = 
    case {TOPPARS1,TOPINIT1,TOPEXPRS} of
      {[],[],[]} -> [];
      _ -> 
        [ {render,?CONF(render),unknown,[],TOPPARS1,TOPINIT1,TOPEXPRS} ]
    end,
  % io:format("Grouping...~n",[]),
  FORMS0 = flatten([TOPFORMS,TOP,group_funs(FRAGS5)]),
  % io:format("render args...~n",[]),
  FORMS1 = guess_render_args(CONFIG,FORMS0),
  % io:format("pack forms...~n",[]),
  pack_forms(CONFIG,FORMS1).
  
group_funs(?OK(CODE)) -> group_funs(CODE);
group_funs(?ERR(ERR)) -> ERR;
group_funs(FRAGS) -> group_funs(FRAGS,[]).
group_funs([FORM={form,_}|FRAGS],ACCUM) -> group_funs(FRAGS,[FORM|ACCUM]);
group_funs([{render,NAME}|FRAGS1],ACCUM) ->
  {PARS,FRAGS2} = lists:splitwith(fun ({param,_})->true; (_)->false end,FRAGS1),
  {INIT,FRAGS3} = lists:splitwith(fun ({init,_})->true; ({param,_,_})->true; (_)->false end,FRAGS2),
  {EXPR,FRAGS4} = lists:splitwith(fun ({expr,_})->true; (E)->is_binary(E) end,FRAGS3),
  group_funs(FRAGS4,[{render,NAME,unknown,[],PARS,INIT,EXPR}|ACCUM]);
group_funs([{render,NAME,ARGS,GUARDS}|FRAGS1],ACCUM) ->
  {INIT,FRAGS2} = lists:splitwith(fun ({init,_})->true; ({param,_,_})->true; (_)->false end,FRAGS1),
  {EXPR,FRAGS3} = lists:splitwith(fun ({expr,_})->true; (E)->is_binary(E) end,FRAGS2),
  group_funs(FRAGS3,[{render,NAME,ARGS,GUARDS,[],INIT,EXPR}|ACCUM]);
group_funs([],ACCUM) -> lists:reverse(ACCUM).

guess_render_args(CONFIG,?OK(CODE)) -> guess_render_args(CONFIG,CODE);
guess_render_args(_CONFIG,?ERR(ERR)) -> ERR;
guess_render_args(CONFIG,FRAGS) -> guess_render_args(CONFIG,FRAGS,[]).
guess_render_args(CONFIG,[{render,NAME,unknown,GUARDS,PARS,INIT,EXPR}|FRAGS],ACCUM) ->
  % io:format("guess_render_args({render,~p,unknown,~p,~p,~p,~p}...)~n",[NAME,GUARDS,PARS,INIT,EXPR]),
  ARGS = (?CONF(args) -- [P || {param,P,_} <- INIT]) ++ [P || {param,P} <- PARS],
  guess_render_args(CONFIG,FRAGS,[{render,NAME,asVars(ARGS),GUARDS,PARS,INIT,EXPR}|ACCUM]);
guess_render_args(CONFIG,[F|FRAGS],ACCUM) -> guess_render_args(CONFIG,FRAGS,[F|ACCUM]);
guess_render_args(_CONFIG,[],ACCUM) -> lists:reverse(ACCUM).

-define(TRACE(TAG,REF),[{trace,{TAG,REF}}]).
-define(TRACE(TAG),?TRACE(TAG,make_ref())).

pack_forms(CONFIG,FRAGS) -> pack_forms(CONFIG,FRAGS,[]).
pack_forms(CONFIG,?OK(CODE),ACCUM) -> pack_forms(CONFIG,CODE,ACCUM);
pack_forms(_CONFIG,?ERR(ERR),_ACCUM) -> ERR;
pack_forms(CONFIG,[{form,FORM}|FRAGS],ACCUM) ->
  % io:format("pack_forms(...{form,~p}...).~n",unparse(FORM)),
  pack_forms(CONFIG,FRAGS,ACCUM ++ [FORM]);
pack_forms(CONFIG,[{render,NAME,ARGS,GUARDS,_,INIT,EXPR}|FRAGS],ACCUM) ->
  % io:format("pack_forms(...{render,~p,~p,~p,_,~p,~p}...).~n",[NAME,ARGS,GUARDS,INIT,EXPR]),
  EXPRTOKENS = flatten(
        [[ {'[',?TRACE('[')}
        , [ [ {',',?TRACE(',')}
            , case E of 
                _ when is_binary(E) -> [{'<<',?TRACE('<<')},{string,?TRACE('string'),binary_to_list(E)},{'>>',?TRACE('>>')}];
                {expr,EX} -> EX
              end
            ] || E <- EXPR ]
        , {']',?TRACE(']')}
        % , {dot,?TRACE(dot)}
        ]]),
  % io:format("~p(~p) . EXPRTOKENS=~p~n",[NAME,ARGS,EXPRTOKENS]),
  EXPRCODE = parse_expcode(CONFIG,EXPRTOKENS),
  % io:format("EXPRTOKENS = ~p~n",[EXPRTOKENS]),
  % io:format("EXPRTOKENS = ~s~n",[untokens(element(2,EXPRTOKENS))]),
  % io:format("EXPRCODE = ~p~n",[EXPRCODE]),
  % io:format("~p(~p) . EXPRCODE=~p~n",[NAME,ARGS,EXPRCODE]),
  case flatten([[case C of
         {init,C} -> C;
         {param,K,V} -> match(var(K),V)
       end || C <- INIT] ,EXPRCODE]) of
    ?OK(CODE) ->
      pack_forms(CONFIG,FRAGS,ACCUM ++ [function(NAME,[clause(ARGS,GUARDS,CODE)])]);
    ?ERR(ERR) -> ERR
  end;
pack_forms(_CONFIG,[_T|_FRAGS],_ACCUM) ->
  % io:format("pack_forms: extra term: ~p~n",[T]), 
  ?ERRINFO(extra_term);
  % pack_forms(CONFIG,FRAGS,ACCUM);
pack_forms(_CONFIG,[],ACCUM) ->
  % io:format("pack_forms(....end) -> ~p~n",[ACCUM]),
  flatten(ACCUM).

parse_expcode(CONFIG,?OK(TOKENS)) -> parse_expcode(CONFIG,TOKENS);
parse_expcode(_CONFIG,?ERR(ERR)) -> ERR;
parse_expcode(CONFIG,TOKENS) ->
  case parse_tokens(exprs,TOKENS) of
    ?ERRINFO(TRACE=?TRACE(',',_),erl_parse,_) ->
      parse_expcode(CONFIG,[T || T<-TOKENS, T/={',',TRACE}]);
    ?OK(_) ->
      ?OK(CODE) = parse_tokens(exprs,[case T of {A,?TRACE(_,_)} -> {A,1}; {A,?TRACE(_,_),B} -> {A,1,B}; X->X end || T <- TOKENS]),
      % io:format("parse_expcode(~p==~s) -> ~p==~s~n",[TOKENS,untokens(TOKENS),CODE,unparse(CODE)]),
      ?OK(CODE);
    ?ERR(ERR) -> {error,{ERR,in,untokens(TOKENS)}}
  end.


asVars(VARS) when is_list(VARS) -> [ asVar(V) || V<-VARS ];
asVars(VARS) ->
  % io:format("asVars(~p)~n",[VARS]),
  VARS.
asVar(VAR) ->
  case asVar_(VAR) of
    not_var -> uvar();
    V -> V
  end.
asVar_(VAR) when is_atom(VAR) -> var(VAR);
asVar_(VAR={var,_,_}) -> VAR;
asVar_({match,_,L,R}) ->
  case asVar_(L) of
    not_var -> asVar_(R);
    VAR -> VAR
  end;
asVar_(_) -> not_var.

parse_frag(CODE) ->
  case parse_frag_(CODE) of 
    ?ERR(ERR) -> {error,{ERR,in,CODE}};
    OK -> OK
  end.
parse_frag_(<<"!",_/binary>>) -> ?OK([]); % comment
parse_frag_(<<"@",CODE/binary>>) -> % starts a new function clause for a render function
  case parse_function_sig(CODE) of
    ?OK({NAME,ARGS,GUARDS}) -> ?OK([{render,NAME,ARGS,GUARDS}]);
    ?OK(NAME) when is_atom(NAME) -> ?OK([{render,NAME}]);
    ?ERR(ERR) -> ERR
  end;
parse_frag_(<<"?",CODE/binary>>) -> % Binds parameters... and other initialisation expressions
  case parse_code(exprs,CODE) of
    ?OK(INITS) ->
      ?OK([ case X of % do the patern matching here to ensure failure if theres nonsense input
              {match,_,{var,_,TAG},VAL} -> {param,TAG,block(VAL)};
              {match,_,{atom,_,TAG},VAL} -> {param,TAG,block(VAL)};
              X -> {init,X}
            end || X <- INITS]);
    ?ERR(ERR) -> ERR
  end;
parse_frag_(<<"+",CODE/binary>>) -> % Add parameter to current (default parameter) function
  case parse_code(args,CODE) of
    ?OK(PARS) -> ?OK([ case X of {var,_,T} -> {param,T} end || X <- PARS]);
    ?ERR(ERR) -> ERR
  end;
parse_frag_(<<"~",CODE/binary>>) -> % Additional Forms
  case parse_code(forms,CODE) of
    ?OK(FORM) -> ?OK([ {form,F} || F<-FORM ]);
    ?ERR(ERR) -> ERR
  end;
parse_frag_(CODE) -> % arbitary erlang expressions that is injected 
  case tokens(CODE) of
    ?OK(TOKS) -> ?OK([{expr,TOKS}]);
    ?ERR(ERR) -> ERR
  end.

parse_function_sig(CODE) when not is_binary(CODE) -> parse_function_sig(iolist_to_binary(CODE));
parse_function_sig(CODE) ->
  try
    case tokens(CODE) of
      ?OK([{atom,_,NAME}]) -> ?OK(NAME);
      ?OK([{atom,_,NAME},{'(',_}|MORE]) ->
        {CLAUSE,RES_1} = lists:splitwith(fun ({'->',_}) -> false; (_)->true end,MORE),
        {ARGS_1,GUARDS_1} = lists:splitwith(fun ({'when',_}) -> false; (_)->true end,CLAUSE),
        case parse_tokens(args,
              case lists:reverse(ARGS_1) of
                [{')',_}|ARGS_2] -> lists:reverse(ARGS_2);
                _ -> erlang:error({parse_function_sig_panic,?ERRINFO(malformed_function)})
              end) of
          ?OK(ARGS) -> ok;
          ?ERR(ERR1) -> erlang:error({parse_function_sig_panic,ERR1}),
                        ARGS=error % so erlang dosen't complain...
        end,
        case GUARDS_1 of
          [{'when',_}|GUARDS_T] ->
             case parse_tokens(guards,GUARDS_T) of
               ?OK(GUARDS) -> ok;
               ?ERR(ERR2) -> erlang:error({parse_function_sig_panic,ERR2}),
                             GUARDS=error % so erlang dosen't complain...
             end;
          [] -> GUARDS = []
        end,
        case RES_1 of
          [{'->',_}|RES_T] ->
             case parse_tokens(exprs,RES_T) of
               ?OK(RES) -> ok;
               ?ERR(ERR3) -> erlang:error({parse_function_sig_panic,ERR3}),
                             RES=error % so erlang dosen't complain...
             end;
          [] -> RES = nothing
        end,
        case RES of
          nothing -> ?OK({NAME,ARGS,GUARDS});
          _ -> ?OK({NAME,ARGS,GUARDS,RES})
        end;
      ?OK(_) -> ?ERRINFO(not_function);
      ?ERR(ERR4) -> ERR4
    end
  catch
    error:{parse_function_sig_panic,ERR5} -> ERR5
  end.


config([{K,V}|_],K) -> V;
config([_|T],K) -> config(T,K);
config([],args) ->
  ['Data'];
config([],render) ->
  'render';
config([],_) -> undefined.



   
  


