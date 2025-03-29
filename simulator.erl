-module(actor).
-import(math, []).
-export[start/0, startActor/1, startActorPushSum/2, startGossip/2, sendGossip/2, sendPushMessages/8].

start() ->
    {ok , [Nodes]}=io:fread("\nEnter number of nodes: ", "~d\n"),
    {ok , [Topology]}=io:fread("\nEnter Topology: ", "~s\n"),
    {ok , [Algorithm]}=io:fread("\nChoose the Algorithm: ", "~s\n"),

    if
        Topology=="2D" ->
            No_of_nodes=getNextSquare(Nodes);
        Topology == "3D" ->
            No_of_nodes=getNextSquare(Nodes);
        true ->
            No_of_nodes=Nodes
    end,
    
    io:format("Nuber of Nodes: ~p\n", [No_of_nodes]),
    io:format("Topology: ~p\n", [Topology]),
    io:format("Algorithm: ~p\n", [Algorithm]),

    case Algorithm of
        "gossip" ->startGossip(No_of_nodes, Topology);
        "pushsum" ->startPushSum(No_of_nodes, Topology)
    end.

startGossip(No_of_nodes, Topology)->
    io:format('Starting the Gossip Algorithm \n'),
    Actors= createActors(No_of_nodes),
    {ChosenActor, ChosenActor_PID}=lists:nth(rand:uniform(length(Actors)), Actors),
    io:format("\nThe Actor chosen by the main process is :~p \n\n", [ChosenActor]),

    Start_Time=erlang:system_time(millisecond),
    ChosenActor_PID !{self(), {Topology, Actors, No_of_nodes}},
    checkAliveActors(Actors),

    io:format("All procceses received the rumor 10 times.\n"),
    End_Time=erlang:system_time(millisecond),
    io:format("\nTime Taken in milliseconds: ~p\n", [End_Time-Start_Time]).

checkAliveActors(Actors) ->
    Alive_Actors=[{A, A_PID} || {A, A_PID} <- Actors, is_process_alive(A_PID)==true],

    if 
        Alive_Actors==[]->
            io:format("\nCONVERED: ");
        true->
            checkAliveActors(Actors)
    end.

getAliveActors(Actors) ->
    Alive_Actors=[{A, A_PID} || {A, A_PID} <- Actors, is_process_alive(A_PID)==true],
    Alive_Actors.


startPushSum(No_of_nodes, Topology)->
    io:format('Start the Push Sum Algorithm \n'),

    W=1,
    Actors=createActorsPushSum(No_of_nodes, W),
    {ChosenActor, ChosenActor_PID}=lists:nth(rand:uniform(length(Actors)), Actors),
    io:format("\nThe Actor chosen is : ~p \n", [ChosenActor]),

    Start_Time=erlang:system_time(millisecond),

    ChosenActor_PID !{self(), {0, 0, Topolgy, Actors, No_of_nodes, self()}},
    checkAliveActors(Actors),
    io:format("All Processes converged with the same s/w ratio.\n"),
    End_Time=erlang:system_time(millisecond),
    io:format("\nTime taken in milliseconds: ~p\n", [End_Time-StartTime]).


buildTopolgy(Topolgy, Actors, No_of_nodes, Id)->
    Actors_Map= maps:from_list(Actros),
    case Topology of 
        "full" -> findFullNetworkNeighbors(Id, No_of_nodes, Actors_Map),
        "2D" -> find2DGridNeighbors(Id, No_of_nodes, Actors_Map),
        "line" -> findLineGridNeighbors(Id, No_of_nodes, Actors_Map),
        "3D" -> find3DGridNeighbors(Id, No_of_nodes, Actors_Map),
    end.

findFullNetworkNeighbors(Id, N, Actors_Map) ->
    Neighbors= lists:subtract(lists:seq(1, N), [Id]), 
    Detailed_Neighbors=[
        {N, maps:get(N, Actors_Map)}
        || N<-Neighbors, maps:is_key(N, Actors_Map)
    ],
    Detailed_Neighbors.