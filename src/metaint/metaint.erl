-module(metaint).

-export([eval/1, eval_lambda/2, eval_apply/2]).

eval_expr({atm, Id}, _) ->
    {ok, Id};
eval_expr({var, Id}, Env) ->
    case find_in_env(Id, Env) of
        false ->
            error;
        {_, Data} ->
            {ok, Data}
    end;
eval_expr({switch, _, _} = Stmt, Env) ->
    eval_switch(Stmt, Env);
eval_expr({lambda, _, _, _} = Stmt, Env) ->
    eval_lambda(Stmt, Env);
eval_expr({abs, _, _, _} = Stmt, Env) ->
    Stmt;
eval_expr({apply, _, _} = Stmt, Env) ->
    eval_apply(Stmt, Env);
eval_expr({cons, Var, Next}, Env) ->
    case eval_expr(Var, Env) of
        error ->
            error;
        {ok, Data} ->
            case eval_expr(Next, Env) of
                error ->
                    error;
                {ok, RecrData} ->
                    {ok, {Data, RecrData}}
            end
    end.

find_in_env(_, []) -> false;
find_in_env(Var, [{HVar, HContent} | TEnv]) ->
    case HVar of
        Var ->
            {HVar, HContent};
        _ ->
            find_in_env(Var, TEnv)
    end.

eval_match(ignore, _, Env) ->
    {ok, Env};
eval_match(Var, {switch, _, _} = Stmt, Env) ->
    eval_match(Var, eval_switch(Stmt, Env), Env);
eval_match({atm, Id}, Id, Env) ->
    {ok, Env};
eval_match({var, Id}, Var, Env) ->
    case find_in_env(Id, Env) of
        false ->
            {ok, [{Id, Var} | Env ]};
        {Id, Var} ->
            {ok, Env};
        {Id, _} ->
            fail
    end;

eval_match({cons, Curr, Next}, {H, T}, Env) ->
    case eval_match(Curr, H, Env) of
        fail ->
            fail;
        {ok, NEnv} ->
            eval_match(Next, T, NEnv)
    end;
eval_match(_, _, _) ->
    fail.

eval_seq([{switch, _, _} = Stmt], Env) ->
    eval_switch(Stmt, Env);
eval_seq([Exp], Env) ->
    eval_expr(Exp, Env);
eval_seq([{match, Ptr, Exp} | Seq], Env) ->
    case eval_expr(Exp, Env) of
        error ->
            error;
        {ok, Str} ->
            case eval_match(Ptr, Str, Env) of
                fail ->
                    error;
                {ok, NEnv} ->
                    eval_seq(Seq, NEnv)
            end
    end.

eval_switch({_, _, {atm, []}}, _) ->
    fail;
eval_switch({switch, Exp, {_, _} = Cmp}, Env) ->
    eval_switch({switch, Exp, {cons, Cmp, {atm, []}}}, Env);
eval_switch({switch, Exp, {cons, {Comp, Seq}, Tail}}, Env) ->
    case eval_expr(Exp, Env) of
        error ->
            fail;
        {ok, Str} ->
            case eval_match(Comp, Str, Env) of
                fail ->
                    eval_switch({switch, Exp, Tail}, Env);
                {ok, NEnv} ->
                    eval_seq(Seq, NEnv)
            end
    end.

eval_lambda({lambda, Free, Param, Seq}, Env) ->
    case lookup_free(Free, Env) of
        error -> error;
        Closure -> {abs, Param, Seq, Closure}
    end.

eval_apply({apply, Expr, Args}, Env) ->
    case eval_expr(Expr, Env) of
        {abs, Param, Seq, Closure} ->
            Nenv = eval_args(Args, Param, Closure),
            eval_seq(Seq, Nenv);
        _ -> error
    end.

eval_args([], [], Closure) -> Closure;
eval_args([ArgH | ArgT], [ParamH | ParamT], Closure) ->
    case eval_match({var, ArgH}, ParamH, []) of
        {ok, [Var]} -> [Var | eval_args(ArgT, ParamT, Closure)];
        _ -> error
    end.

lookup_free([], _) -> [];
lookup_free([FreeHead | Tail], Env) ->
    case find_in_env(FreeHead, Env) of
        false -> error;
        Var -> [Var | lookup_free(Tail, Env)]
    end.

eval(Seq) ->
    eval_seq(Seq, []).

