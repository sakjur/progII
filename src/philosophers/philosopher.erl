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
    spawn_link(fun() -> dreaming(Philosopher) end).

dreaming(Philosopher) ->
    sleep(500, 250),
    wakeup(Philosopher).

wakeup(Philosopher = #philosopher{hungry=0}) -> 
    io:format("~s is full.~n", [Philosopher#philosopher.name]),
    done;
wakeup(Philosopher) ->
    Name = Philosopher#philosopher.name,
    L = Philosopher#philosopher.l,
    R = Philosopher#philosopher.r,
    io:format("~s woke up.~n", [Name]),
    chopstick:request(R),
    chopstick:request(L),
    case chopstick:granted() of
        {ok, StickA} ->
            case chopstick:granted() of
                {ok, _} ->
                    io:format("~s got chopsticks~n", [Name]),
                    eating(Philosopher);
                no ->
                    chopstick:return(StickA),
                    dreaming(Philosopher)
            end;
        no ->
            dreaming(Philosopher)
    end.

eating(Philosopher) ->
    Name = Philosopher#philosopher.name,
    L = Philosopher#philosopher.l,
    R = Philosopher#philosopher.r,
    Hunger = Philosopher#philosopher.hungry,
    sleep(700, 250),
    io:format("~s has eaten.~n", [Name]),
    chopstick:return(R),
    chopstick:return(L),
    dreaming(Philosopher#philosopher{hungry=Hunger-1}).

sleep(T, D) ->
    random:seed(now()),
    RandVal = random:uniform(D),
    timer:sleep(T + RandVal).

