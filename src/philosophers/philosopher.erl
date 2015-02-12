-module(philosopher).

-export([start/5]).

-record(philosopher, { hungry,  r, l, name, ctrl }).

start(Hungry, R, L, Name, Ctrl) ->
    Philosopher = #philosopher{hungry=Hungry, r=R, l=L, name=Name, ctrl=Ctrl},
    spawn_link(fun() -> initialize(Philosopher) end).

initialize(Philosopher) ->
    random:seed(now()),
    dreaming(Philosopher).

dreaming(Philosopher) ->
    sleep(50, 50),
    wakeup(Philosopher).

wakeup(Philosopher) ->
    Name = Philosopher#philosopher.name,
    L = Philosopher#philosopher.l,
    R = Philosopher#philosopher.r,
    chopstick:request(R),
    chopstick:request(L),
    io:format("Requested chopsticks: ~s ~n", [Name]),
    case chopstick:granted() of
        {ok, StickA} ->
            case chopstick:granted() of
                {ok, _} ->
                    io:format("Chopsticks granted: ~s ~n", [Name]),
                    eating(Philosopher);
                no ->
                    chopstick:return(StickA),
                    dreaming(Philosopher)
            end;
        no ->
            case chopstick:granted() of
                {ok, StickB} ->
                    chopstick:return(StickB);
                no ->
                    na
            end,
            dreaming(Philosopher)
    end.

eating(Philosopher) ->
    Name = Philosopher#philosopher.name,
    L = Philosopher#philosopher.l,
    R = Philosopher#philosopher.r,
    sleep(200, 250),
    io:format("Has eaten: ~s ~n", [Name]),
    chopstick:return(R),
    chopstick:return(L),

    Hunger = Philosopher#philosopher.hungry - 1,
    case Hunger of
        0 -> 
            io:format("Full: ~s ~n", [Name]), 
            Philosopher#philosopher.ctrl ! done,
            done;
        _ ->
            dreaming(Philosopher#philosopher{hungry=Hunger})
    end.

sleep(T, D) ->
    RandVal = random:uniform(D),
    Time = T + RandVal,
    % io:format("Sleeping ~w ms ~n", [Time]),
    timer:sleep(Time).

