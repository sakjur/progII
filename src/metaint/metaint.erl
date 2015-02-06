-module(metaint).

-export([eval/1]).

eval_expr({atm, Id}, _) ->
    {ok, Id};
eval_expr({var, Id}, Env) ->
    case find_in_env(Id, Env) of
        false ->
            error;
        {_, Data} ->
            {ok, Data}
    end;
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

eval(Seq) ->
    eval_seq(Seq, []).

