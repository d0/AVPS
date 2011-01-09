%%2011 Dominik Oepen, Jan Birkholz
-module(maekawa).
-export([init/1, new_node/0]).

init(Num_procs) ->
    %Create Num_proc processes
    Procs = create_procs(Num_procs, []),
    %Create sqr(Num_procs) groups of processes
    Num_groups = ceiling(math:sqrt(Num_procs)),
    Groups = create_groups(Num_groups - 1, Procs, []),
    io:format("Gruppen: ~w~n", [Groups]),
    %overlap the groups
    Overlapped = overlap(lists:nth(1, Procs), Groups, []),

    lists:foreach(fun(Elem) -> lists:foreach(fun(Elem2) -> Elem2 ! Elem end,Elem) end,Overlapped),
    %Send a message to process 2 in order to start Maekawa algorithm
    lists:nth(2, Procs) ! go.
    

create_procs(Num, List) ->
    if Num > 0 ->
        Pid = spawn(maekawa, new_node, []),
        create_procs(Num-1, lists:append(List, [Pid]));
    Num == 0 ->
        List
    end.

create_groups(Group_size, Procs, Groups) when Group_size =< length(Procs) ->
    Group = lists:sublist(Procs, Group_size),
    create_groups(Group_size, lists:nthtail(Group_size, Procs), lists:append(Groups,[Group]));

create_groups(Group_size, Procs, Groups) when Group_size > length(Procs) ->
    case length(Procs) of
        0 -> Groups;
        _ -> lists:append(Groups, [Procs])
    end.


overlap(PID, [Head | Rest], Result) ->
    case lists:member(PID, Head) of
        false ->
            New = lists:append([PID], Head),
            overlap(PID, Rest, lists:append(Result, [New]));
        true ->
            overlap(PID, Rest, lists:append(Result, [Head]))
    end;

overlap(_PID, [], Result) ->
    Result.

new_node() ->
    receive Groupmembers ->
            %io:format("~w: Group ~w~n",[self(),Groupmembers])
            nothing
    end,
    %State = released,
    %Voted = false,
    %Wait for a message before entering critical section
    new_node(Groupmembers).
    %aquire_mutex().

new_node(Groupmembers) ->
	receive 
       		Msg ->
			case Msg of 
				go ->
					io:format("~p: Mutex: going crit~n",[self()]),
    					aquire_mutex(Groupmembers);
				{request,Pid} ->
					io:format("~p: Mutex?: got request~n",[self()]),
					Pid ! {reply,self()};
				%ignore any more Groupmember Messages the overlapping elem gets
				_ ->
					%io:format("~p: ignoring ~p~n",[self(),Msg])
					nothing
		end
    	end,
	new_node(Groupmembers).


aquire_mutex(Groupmembers) ->
    io:format("~p: Mutex: Waiting is over~n", [self()]),
    %nothing.
    %State = wanted,
    lists:foreach(fun(Elem) -> Elem ! {request,self()} end,[X || X<-Groupmembers, X=/=self()]),
    getreplies([X || X<-Groupmembers, X=/=self()]),
    held.

getreplies([]) -> 
	[],
	io:format("~p: Mutex: got all replies~n",[self()]);

getreplies(Groupmembers) ->
	receive 
		{reply,Pid} ->
			io:format("~p: Mutex: received reply from ~p~n",[self(),Pid])
	end,
	getreplies(lists:delete(Pid,Groupmembers)).


ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
