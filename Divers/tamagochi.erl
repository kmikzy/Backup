-module(tamagochi).
-export([start/0, loop/1]).
-define(username, os:getenv("USERNAME")).

%% Design of a Tamagochi
-record(tamagochi, {hunger, mood, affection}).

%% Extract Tamagochi data
tamagochi(Tamagochi) ->
    H = Tamagochi#tamagochi.hunger,
    M = Tamagochi#tamagochi.mood,
    A = Tamagochi#tamagochi.affection,
    {H, M, A}.

%% Build Tamagochi
tamagochi(Hunger, Mood, Affection) ->
    #tamagochi{
      hunger = Hunger,
      mood = Mood,
      affection = Affection
    }.

%% start process
start()->
    Tamagochi = tamagochi(100, normal, 100),
    Pid = spawn(?MODULE, loop, [Tamagochi]),
    register(tamagochi, Pid),
    Pid.

%% Check Hungry
check_hungry(Hunger) when Hunger < -200 ->
    io:format("... rip~n"),
    tamagochi ! {stop, "Dead"};
check_hungry(Hunger) when Hunger < -100 ->
    io:format("Please ~p... i'm Hungry ~n",[?username]);
check_hungry(Hunger) -> ok.
    

%% Loop process
loop(Tamagochi)->
    {Hunger, Mood, Affection} = tamagochi(Tamagochi),
    receive
	stop -> exit("End");
	{stop, Reason} ->
	    dead(),
	    exit(Reason);
	feed -> 
	    NewTamagochi = #tamagochi{
	      hunger = Hunger+1,
	      mood = Mood,
	      affection = Affection +1
	     },
	    loop(NewTamagochi)
    after 3 ->
	    check_hungry(Hunger),
	    NewTamagochi = #tamagochi{
	      hunger = Hunger-1,
	      mood = Mood,
	      affection = Affection +1
	     },
	    loop(NewTamagochi)
    end.
    
dead() ->
    io:format("Oh... it's so sad ~p, your beast 's dead...~n", [?username]),
    ok.
