-module(actor).
-import(math, []).
-export([start/0, startActors/1, startActorsPushSum/2, startGossip/2, sendGossip/5, sendPushSumMessages/8]).

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
    
    io:format("Number of Nodes: ~p\n", [No_of_nodes]),
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
            io:format("\nCONVERGED: ");
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

    ChosenActor_PID !{self(), {0, 0, Topology, Actors, No_of_nodes, self()}},
    checkAliveActors(Actors),
    io:format("All Processes converged with the same s/w ratio.\n"),
    End_Time=erlang:system_time(millisecond),
    io:format("\nTime taken in milliseconds: ~p\n", [End_Time-Start_Time]).


buildTopology(Topology, Actors, No_of_nodes, Id)->
    Actors_Map= maps:from_list(Actors),
    case Topology of 
        "full" -> findFullNetworkNeighbors(Id, No_of_nodes, Actors_Map);
        "2D" -> find2DGridNeighbors(Id, No_of_nodes, Actors_Map);
        "line" -> findLineGridNeighbors(Id, No_of_nodes, Actors_Map);
        "3D" -> findImperfect3DGridNeighbors(Id, No_of_nodes, Actors_Map)
    end.

findFullNetworkNeighbors(Id, N, Actors_Map) ->
    Neighbors= lists:subtract(lists:seq(1, N), [Id]), 
    Detailed_Neighbors=[
        {Nei, maps:get(Nei, Actors_Map)}
        || Nei<-Neighbors, maps:is_key(Nei, Actors_Map)
    ],
    Detailed_Neighbors.

find2DGridNeighbors(Id, N, Actors_Map)->

    Rows=erlang:trunc(math:sqrt(N)),
    ModVal=Id rem Rows,

    if 
        ModVal==1->
            Neighbors=[Id+1];
        ModVal==0->
            Neighbors=[Id-1];

        true->
            Neighbors=lists:append([Id-1], [Id+1])
    end,

    if Id+Rows>N->
        Neighbors2=Neighbors;
    true->
        Neighbors2=lists:append([Neighbors, [Id+Rows]])
    end,

    if Id-Rows<1->
        Neighbors3=Neighbors2;
    true->
        Neighbors3=lists:append([Neighbors2, [Id-Rows]])
    end,

    Detailed_Neighbors=[
        {Nei, maps:get(Nei, Actors_Map)}
        || Nei <- Neighbors3, maps:is_key(Nei, Actors_Map)
    ],
    Detailed_Neighbors.

findLineGridNeighbors(Id, N, Actors_Map)->
    if 
        Id>N->
            Neighbors=[];
        Id<1 ->
            Neighbors=[];
        Id+1>N->
            if 
                Id-1<1->
                    Neighbors=[];
                true->
                    Neighbors=[Id-1]
            end;
        true->
            if 
                Id-1<1->
                Neighbors=[Id+1];
            true ->
                Neighbors=[Id-1, Id+1]
            end
    end,

    Detailed_Neighbors=[
        {Nei, maps:get(Nei, Actors_Map)}
        || Nei <- Neighbors, maps:is_key(Nei, Actors_Map)
    ],
    Detailed_Neighbors.

findImperfect3DGridNeighbors(Id, N, Actors_Map)->
    Rows=erlang:trunc(math:sqrt(N)),
    ModVal=Id rem Rows,

    if 
        ModVal==1->
            Neighbors=[Id+1];
        ModVal==0->
            Neighbors=[Id-1];

        true->
            Neighbors=lists:append([Id-1], [Id+1])
    end,

    if Id+Rows>N->
        Neighbors2=Neighbors;
    true->
        if 
            ModVal==1->
                Neighbors2=lists:append([Neighbors, [Id+Rows], [Id+Rows+1]]);
            ModVal==0->
                Neighbors2=lists:append([Neighbors, [Id+Rows], [Id+Rows-1]]);
            true->
                Neighbors2=lists:append([Neighbors, [Id+Rows], [Id+Rows-1], [Id+Rows+1]])
        end
    end,
    if
        Id-Rows < 1 ->
            ImmediateNeighbors = Neighbors2;
        true ->
            if 
                ModVal == 1 ->
                    ImmediateNeighbors = lists:append([Neighbors2, [Id-Rows], [Id-Rows+1]]);
                ModVal == 0 ->
                    ImmediateNeighbors = lists:append([Neighbors2, [Id-Rows], [Id-Rows-1]]);
                true ->
                    ImmediateNeighbors = lists:append([Neighbors2, [Id-Rows], [Id-Rows-1], [Id-Rows+1]])
            end
    end,

    NeighborsToBeIgnored=lists:append([ImmediateNeighbors, [Id]]),
    RemainingNeighbors= lists:subtract(lists:seq(1, N), NeighborsToBeIgnored),

    RandomRemainingNeighbor= lists:nth(rand:uniform(length(RemainingNeighbors)), RemainingNeighbors),

    FinalNeighbors= lists:append([[RandomRemainingNeighbor], ImmediateNeighbors]),

    Detailed_Neighbors=[
        {Nei, maps:get(Nei, Actors_Map)}
        || Nei<-FinalNeighbors, maps:is_key(Nei, Actors_Map)
    ],
    Detailed_Neighbors.

startActors(Id)->
    awaitResponseGossip(Id, 0).

awaitResponseGossip(Id, Count)->
    receive
      {From, {Topology, Actors, No_of_nodes}} ->
        if 
            Count ==10->
                timer:sleep(2000),
                exit(0);
            true->
                spawn(actor, sendGossip, [self(), Topology, Actors, No_of_nodes, Id]),
                awaitResponseGossip(Id, Count+1)
            end
    end.

sendGossip(Current, Topology, Actors, No_of_nodes, Id)->
    Status=is_process_alive(Current),
    if 
        Status==true->
            Alive_Actors=getAliveActors(Actors),
            Neighbors=buildTopology(Topology, Alive_Actors, No_of_nodes, Id),

            if 
                Neighbors==[]->
                    exit(0);
                true->
                    {_, ChosenNeighbor_PID}=lists:nth(rand:uniform(length(Neighbors)), Neighbors),
                    ChosenNeighbor_PID ! {Current, {Topology, Actors, No_of_nodes}},
                    sendGossip(Current, Topology, Actors, No_of_nodes, Id)
                end;
            true->
                exit(0)
            end.

createActors(N)->
    Actors=[
        {Id, spawn(actor, startActors, [Id])}
        || Id <- lists:seq(1, N)
    ],
    Actors.

createActorsPushSum(N, W)->
    Actors=[
        {Id, spawn(actor, startActorsPushSum, [Id, W])}
        || Id <- lists:seq(1, N)
    ],
    Actors.

startActorsPushSum(Id, W)->
    awaitResponsePushSum(Id, Id, W, 0, 0, self()).

awaitResponsePushSum(Id, S, W, Prev_ratio, Count, Last_Spawned_Process_Id)->
    receive
      {From, {S1, W1, Topology, Actors, No_of_nodes, Main}} ->
        if 
            Count>5->
                timer:sleep(1000),
                exit(0);
            true->
                S2=S+S1,
                W2=W+W1,

                Alive_Actors=getAliveActors(Actors),
                Neighbors = buildTopology(Topology, Alive_Actors, No_of_nodes, Id),

                if 
                    Neighbors==[]->
                        exit(0);
                    true->
                        S3=S2/2,
                        W3=W2/2,

                        if From==Main->
                            Spawned_Process_Id=spawn(actor, sendPushSumMessages, [self(), Actors, Topology, No_of_nodes, Id, S3, W3, Main]);
                        true->
                            Last_Spawned_Process_Id ! {exit},
                            Spawned_Process_Id=spawn(actor, sendPushSumMessages, [self(), Actors, Topology, No_of_nodes, Id, S3, W3, Main])
                        end,

                        Curr_ratio=S/W,
                        Difference = math:pow(10, -10),


                        if 
                            abs(Curr_ratio-Prev_ratio)<Difference->
                                % awaitResponsePushSum(Id, S3, W3, Curr_ratio, Count+1, Spawned_Process_Id);
                                exit(0);
                            true->
                                awaitResponsePushSum(Id, S3, W3, Curr_ratio, 0, Spawned_Process_Id)

                            end
                        end
                    end
    end.


sendPushSumMessages(Current, Actors, Topology, No_of_nodes, Id, S3, W3, Main) ->
    Status = is_process_alive(Current),
if 
    Status == true ->
        Alive_Actors = getAliveActors(Actors),
        Neighbors = buildTopology(Topology, Alive_Actors, No_of_nodes, Id),

        if Neighbors == [] ->
            exit(0);
        true ->
            {_, ChosenNeighbor_PID} = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
            ChosenNeighbor_PID ! {self(), {S3, W3, Topology, Actors, No_of_nodes, Main}},
            sendPushSumMessages(Current, Actors, Topology, No_of_nodes, Id, S3, W3, Main)
        end;
    true ->
        exit(0)
end.

          
getNextSquare(No_of_nodes)->
    SquaredNumber=round(math:pow(math:ceil(math:sqrt(No_of_nodes)), 2)),
    SquaredNumber.






