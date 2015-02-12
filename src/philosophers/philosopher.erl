-module(philosopher).

-export([start/5]).

-record(philosopher,
    {
        hungry,
        r,
        l,
        name,
        ctrl
    }).

start(Hungry, R, L, Name, Ctrl) ->
    Philosopher = #philosopher{hungry=Hungry, r=R, l=L, name=Name, ctrl=Ctrl},
    spawn_link(fun() -> initialize(Philosopher) end).

initialize(Philosopher) ->
    random:seed(now()),
    dreaming(Philosopher).

dreaming(Philosopher) ->
    sleep(500, 250),
    wakeup(Philosopher).

wakeup(Philosopher) ->
    Name = Philosopher#philosopher.name,
    L = Philosopher#philosopher.l,
    R = Philosopher#philosopher.r,
    io:format("Requests chopsticks: ~s ~n", [Name]),
    case chopstick:request(R) of
        ok ->
        case chopstick:request(L) of
            ok ->
                io:format("~s got chopsticks~n", [Name]),
                eating(Philosopher);
            no ->
                chopstick:return(R),
                dreaming(Philosopher)
        end;
        no ->
            dreaming(Philosopher)
    end.

eating(Philosopher) ->
    Name = Philosopher#philosopher.name,
    L = Philosopher#philosopher.l,
    R = Philosopher#philosopher.r,
    sleep(700, 250),
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
    timer:sleep(T + RandVal).

