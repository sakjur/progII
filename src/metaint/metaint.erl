-module(metaint).

-export([eval_expr/2]).

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

