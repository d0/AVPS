%%2011 Dominik Oepen, Jan Birkholz
-module(paxos).
-export([start_proposer/3, start_acceptor/1, start_learner/1, test1/0,testa/0, testb/0,testc/0,testd/0]).

%%%%%%%%%%%%%%%%% Proposer %%%%%%%%%%%%%%%%%%%%%%%%%%%

%Start proposer with a given list of acceptors (and learners?) OR wait for a message including this list: atm given list
start_proposer(Value,R,Acceptors) ->
	%Init r=1, r_latest = 0, latest=none, ack_num=0
	io:format("~p: Proposer(v=~p,r=~p)~n",[self(),Value,R]),
	propose(Value, R, 0, none, Acceptors).

propose(Value, R, R_latest, Latest, Acceptors) ->
	lists:foreach(fun(Acceptor) -> Acceptor ! {prepare, self(), R} end, Acceptors),
	proposer_get_acks(Value, R, R_latest, Latest, 0, Acceptors), %ack_num = 0
	nothing.

%Get acks
proposer_get_acks(Value, R, R_latest, Latest, Ack_num, Acceptors) -> 
	%io:format("Ack_num: ~w~n", [Ack_num]),
	case Ack_num >= length(Acceptors) of
		true ->
			case Latest of
				none -> %Propose own value
					proposer_accept(Value, R, Acceptors);
				_ ->
					proposer_accept(Latest, R, Acceptors)
			end;

		false ->
			nothing
	end,

	receive {ack, R_ack, Value_i, R_i} ->
			case (R == R_ack) of
				true ->
					%io:format("R_i:~w, R_latest:~w~n", [R_i, R_latest]),
					case (R_i > R_latest) of
						true ->
							%io:format("Proposer got ACK~n"),
							proposer_get_acks(Value, R, R_i, Value_i, Ack_num + 1, Acceptors);
						false ->
							proposer_get_acks(Value, R, R_latest, Latest, Ack_num + 1, Acceptors)
					end;
				false ->
					proposer_get_acks(Value, R, R_latest, Latest, Ack_num, Acceptors)
			end
	end.

%Send accepts
proposer_accept(Value, R, Acceptors) ->
	%io:format("Proposer accept~n"),
	lists:foreach(fun(Acceptor) -> Acceptor ! {accept, Value, R} end, Acceptors).

%%%%%%%%%%%%%%%%% Acceptor %%%%%%%%%%%%%%%%%%%%%%%%%%%
start_acceptor(Learners) ->
	%init R_ack = 0, R_accepted = 0, Value = none
	acceptor(0, 0, none, Learners).

acceptor(R_ack, R_accepted, Value, Learners) ->
	receive
		Msg ->
			case Msg of
				{prepare, Pid, R} -> %Phase 1
					case (R > R_ack) and (R > R_accepted) of
						true ->
							Pid ! {ack, R, Value, R_accepted},
							acceptor(R, R_accepted, Value, Learners);
						false ->
							%NACK here or timeout in proposer: atm timeout
							acceptor(R_ack, R_accepted, Value, Learners) 
					end;
				{accept, R, W} -> %Phase 2
					io:format("~p: Acceptor: Got accept message~n",[self()]),
					case (R > R_ack) and (R > R_accepted) of
						true ->
							acceptor_accepted(R, W, Learners),
							acceptor(R_ack, R, W, Learners);
						false ->
							acceptor(R_ack, R_accepted, Value, Learners)
					end
			end
	end.


acceptor_accepted(R_accepted, W, Learners) ->
	lists:foreach(fun(Learner) -> Learner ! {accepted, R_accepted, W} end, Learners).

%%%%%%%%%%%%%%%%% Learner %%%%%%%%%%%%%%%%%%%%%%%%%%%
start_learner(Maj) ->
	%Init num_acceptors = 0, R = 0, Value = none
	learner(0, 0, none, Maj,false,0).

learner(Num_accepted, R_lcl, V_lcl, Maj,Consensus,Result) ->
	if 
		(Num_accepted >= Maj) ->
			io:format("~p: Learner: Value accepted: ~w~n", [self(),R_lcl]),
			receive
				Msg ->
					case Msg of
						{accepted, R, Value} ->
							io:format("Learner received accepted msg:~p:~p~n",[R,Value]),
							if
								(R > R_lcl) ->
									io:format("New value"),
									learner(1, R, Value, Maj,true,R_lcl);
								(R =< R_lcl) ->
									learner(Num_accepted + 1, R_lcl, Value, Maj,true,R_lcl)
							end; 
					_ ->
							%io:format("WTF?~n"),
							nothing
					end
			end,
			nothing;
		(Num_accepted < Maj) ->
			io:format("~p: Learner: Value not accepted: ~p (~p/~p)~n",[self(),R_lcl,Num_accepted,Maj]),
			receive
				Msg ->
					case Msg of
						{accepted, R, Value} ->
							%io:format("Learner received accepted msg:~p:~p~n",[R,Value]),
							if
								(R > R_lcl) ->
									%io:format("New value",[]),
									learner(1, R, Value, Maj,Consensus,Result);
								(R =< R_lcl) ->
									learner(Num_accepted + 1, R_lcl, Value, Maj,Consensus,Result)
							end; 
					_ ->
							%io:format("WTF?~n"),
							nothing
					end
			after 2000->
					if
						(Num_accepted < Maj) and (Consensus == false) ->
							io:format("~p: Learner: No consensus found (Timeout)~n",[self()]);
						(Num_accepted < Maj) and (Consensus == true) ->
							io:format("~p: Learner: Old Consensus still true: ~p~n",[self(),Result]);
							nothing;
						(Num_accepted >= Maj) ->
							nothing
					end
			end
	end.


%%%%%%%%%%%%%%%%% Test cases %%%%%%%%%%%%%%%%%%%%%%%%%%%
test1() ->
	%3 acceptors, 1 learner, 1 proposer, no crash
	Learner = spawn(paxos, start_learner, [3]),
	Acceptor1 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor2 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor3 = spawn(paxos, start_acceptor, [[Learner]]),
	Proposer = spawn(paxos, start_proposer, [23, 1, [Acceptor1, Acceptor2, Acceptor3]]),
	nothing.

testa() ->
	%3 acceptors, 1 learner, 2 proposer
	Learner = spawn(paxos, start_learner, [2]),
	Acceptor1 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor2 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor3 = spawn(paxos, start_acceptor, [[Learner]]),
	
	%If Proposers are started with all acceptors, consensus will be found first for the first proposer and then for the second
	Proposer = spawn(paxos, start_proposer, [23, 1, [Acceptor2]]),
	Proposer2 = spawn(paxos, start_proposer, [24, 1, [Acceptor1, Acceptor3]]),
	nothing.

testb() ->
	%3 acceptors, 1 learner, 1 proposer; majority=2, minority=1
	Learner = spawn(paxos, start_learner, [2]),
	Acceptor1 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor2 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor3 = spawn(paxos, start_acceptor, [[Learner]]),
	Proposer = spawn(paxos, start_proposer, [23, 1, [Acceptor2, Acceptor3]]),
	nothing.

testc() ->
	%3 acceptors, 1 learner, 1 proposer; minority=1 - no consensus (Timeout=2s)
	Learner = spawn(paxos, start_learner, [2]),
	Acceptor1 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor2 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor3 = spawn(paxos, start_acceptor, [[Learner]]),
	Proposer = spawn(paxos, start_proposer, [23, 1, [Acceptor1]]),
	nothing.

testd() ->
	%3 acceptors, 1 learner, 1 proposer; majority=2, minority=1
	Learner = spawn(paxos, start_learner, [2]),
	Acceptor1 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor2 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor3 = spawn(paxos, start_acceptor, [[Learner]]),
	Proposer = spawn(paxos, start_proposer, [23, 1, [Acceptor2, Acceptor3]]),
	%2nd proposer with minority just gets back the result from the first (majority) proposer
	%'ask' by minority or lesser round nr
	Proposer2 = spawn(paxos, start_proposer, [24, 1, [Acceptor1]]),
	nothing.

%%%%%%%%%%%%%%%%% Utility funs %%%%%%%%%%%%%%%%%%%%%%%%%%%
sleep(Ms) ->
	receive
	after Ms ->
			io:format("slept ~p ms~n",[Ms]),
			nothing
	end.

