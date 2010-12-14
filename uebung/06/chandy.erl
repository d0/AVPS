-module(chandy).
-export([start/0,active_proc/2,passive_proc/1]).

print_partners([]) ->
    io:format("~n",[]);
print_partners([Head | Tail]) ->
    io:format("~w is connected with ~w~n", [self(), Head]),
    print_partners(Tail).

send_pid(Partners) ->
    lists:foreach(fun(P) -> P ! {pids, lists:delete(P, Partners)} end, Partners).

print_state([Head | Tail]) ->
    io:format("~w~n", [Head]),
    print_state(Tail);
print_state([]) ->
    nothing.

%The process that starts Chandy-Lamport
active_proc({Schrauben, Euro}, Partners) ->
    io:format("Active process started. PID: ~w~n", [self()]),
    print_partners(Partners),
    %Send the PIDs to all processes
    send_pid([self() | Partners]), 
    %Start Chandy-Lamport algorithm
    State = chandy_lamport({Schrauben, Euro}, Partners, Partners),
    print_state(State).

%A process that doesn't start Chandy-Lamport
passive_proc({Schrauben, Euro}) ->
    io:format("Passive Process started: PID:~w~n", [self()]),
    receive
        {pids, Partners} ->
            lists:delete(self(), Partners),
            io:format("~w: Received list of partners~n", [self()]),
            print_partners(Partners),
            %Send a random amount of screws to the
            %the first process in the Partners list
            [First | Rest] = Partners,
            Rand = random:uniform(Schrauben),
            First ! {self(), {Rand, 0}},
            passive_proc({Schrauben - Rand, Euro}, Partners)
    end.

passive_proc({Schrauben, Euro}, Partners) ->
    receive
        {PID, chandy} ->
            io:format("~w received marker from ~w~n", [self(), PID]),
            State = chandy_lamport({Schrauben, Euro}, lists:delete(PID, Partners), Partners),
            print_state(State)
    end.

%Send markers over all outgoing channels
send_markers([First | Rest]) ->
    if
        First == self() ->
            send_markers(Rest);
        true -> %else
            First ! {self(), chandy}, 
            send_markers(Rest)
    end;
send_markers([]) ->
    nothing.

record_traffic(Incoming) ->
    record_traffic(Incoming, []).
record_traffic([], Messages) ->
    io:format("~w has received markers from all other processes~n", [self()]),
    Messages;
record_traffic(Incoming, Messages) ->
    receive
        %Normal message: Record it and keep on listening
        {Pid, {Schrauben, Euro}} ->
            record_traffic(Incoming, lists:append(Messages, [{msg, Pid, {Schrauben, Euro}}]));
        %Marker received. Stop recording and return empty list (no messages received)
        {Pid, chandy} ->
            io:format("~w has received a marker from process ~w~n", [self(), Pid]),
            record_traffic(lists:delete(Pid, Incoming), Messages)
    end.

%Perform Chandy-Lampert algorith:
%Parameters: Own state ({Schrauben, Euro}), list of all outgoing channels,
%   list of all incoming channels, channel on which the first marker was
%   received
chandy_lamport({Schrauben, Euro}, Incoming, Outgoing) ->
    io:format("Process ~w is starting the Chandy-Lamport algorithm~n", [self]),
    %Record state
    State = {state, self(), {Schrauben, Euro}},
    %Send marker to all processes
    send_markers(Outgoing),
    %Record messages on all inbound channels
    Recorded_messages = record_traffic(Incoming),
    io:format("~w has finished taking a snapshot~n", [self()]),
    [State | Recorded_messages].

start() ->
    P2_PID = spawn(chandy, passive_proc, [{2000, 50}]),
    P3_PID = spawn(chandy, passive_proc, [{1337, 23}]),
    spawn(chandy, active_proc, [{0,900},[P2_PID, P3_PID]]).
