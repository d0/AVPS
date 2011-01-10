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
    lists:nth(3, Procs) ! go,
    sleep(2000), %Procs should not aquire mutex at the same time -> waiting endless for replies
    lists:nth(2,Procs) ! go,
    sleep(2000),
    lists:nth(3, Procs) ! stop,
    nothing.
    
sleep(Ms) ->
	receive
	after Ms ->
			io:format("slept ~p ms~n",[Ms]),
			nothing
	end.

%create processes and divide them into groups
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


%make Vi, Vj overlapping by giving each group one shared element
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


%Maekawa process logic
new_node() ->
    receive Groupmembers ->
            %io:format("~w: Group ~w~n",[self(),Groupmembers])
            nothing
    end,
    %Wait for a message before entering critical section
    new_node(Groupmembers,{released,false},[]). %{State,Voted}

new_node(Groupmembers, {State, Voted}, Queue) ->
	%{State, Voted} = States,
	%io:format("~p: State {~p,~p}~n",[self(),State, Voted]),
	receive 
       		Msg ->
			case Msg of 
				go ->
					%io:format("~p: Mutex: going crit~n",[self()]),
					io:format("~p: {~p,~p} -> {~p,~p}~n",[self(),State, Voted, wanted,Voted]),
					new_node(Groupmembers, aquire_mutex(Groupmembers,{wanted,Voted}),Queue);
				stop ->
					%io:format("~p: Mutex: going noncrit~n",[self()]),
					io:format("~p: {~p,~p} -> {~p,~p}~n",[self(),State, Voted, released,Voted]),
					new_node(Groupmembers, release_mutex(Groupmembers,{released,Voted}),Queue);
				{request,Pid} ->
					%io:format("~p: Mutexrequest~n",[self()]),
					if 
						(Voted == true) or (State == held) -> 
							%io:format("~p: queue request from pj without replying~n",[self()]),
							NewQueue = lists:append(Queue, [Pid]),
							io:format("~p: ~p -> ~p~n",[self(),Queue,NewQueue]),
							new_node(Groupmembers, {State, Voted}, NewQueue);
						%always matches
						true -> 
							Pid ! {reply,self()},
							io:format("~p: {~p,~p} -> {~p,~p}~n",[self(),State, Voted, State, true]),
							new_node(Groupmembers, {State, true},Queue)
							%io:format("~p: {~p,~p} ~n",[self(),State, Voted])
					end;
				{release,Pid} ->
					if
						(Queue /= []) ->
							[H|T] = Queue,
							H ! {reply,self()},
							io:format("~p: ~p -> ~p~n",[self(),Queue,T]),
							io:format("~p: {~p,~p} -> {~p,~p}~n",[self(),State, Voted, State, true]),
							new_node(Groupmembers, {State, true}, T);
						(Queue == []) ->
							io:format("~p: {~p,~p} -> {~p,~p}~n",[self(),State, Voted, State, false]),
							new_node(Groupmembers, {State, false}, Queue)
					end,
					nothing;
				%ignore any more Groupmember Messages the overlapping elem gets
				_ ->
					%io:format("~p: ignoring ~p~n",[self(),Msg])
					nothing
		end
    	end,
	new_node(Groupmembers, {State, Voted}, Queue).


aquire_mutex(Groupmembers, {State, Voted}) ->
    %io:format("~p: Mutex: Waiting is over~n", [self()]),
    lists:foreach(fun(Elem) -> Elem ! {request,self()} end,[X || X<-Groupmembers, X=/=self()]),
    getreplies([X || X<-Groupmembers, X=/=self()]),
    io:format("~p: Critical~n", [self()]),
    io:format("~p: {~p,~p} -> {~p,~p}~n",[self(),State, Voted, held, Voted]),
    {held,Voted}.

release_mutex(Groupmembers, {State, Voted}) ->
    %io:format("~p: Mutex: Releasing~n", [self()]),
    lists:foreach(fun(Elem) -> Elem ! {release,self()} end,[X || X<-Groupmembers, X=/=self()]),
    io:format("~p: Noncritical~n", [self()]),
    {State,Voted}.

%wait for replies to enter critical section
getreplies([]) -> 
	[],
	%io:format("~p: Mutex: got all replies~n",[self()]);
	nothing;

getreplies(Groupmembers) ->
	receive 
		{reply,Pid} ->
			%io:format("~p: Mutex: received reply from ~p~n",[self(),Pid])
			getreplies(lists:delete(Pid,Groupmembers))
	%after 5000 ->
	%		io:format("~p: Deadlock~n",[self()]) %we get a deadlock if 2 procs of the same group want to got crit
	end.


ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
