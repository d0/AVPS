%%2011 Dominik Oepen, Jan Birkholz
-module(paxos).
-export([start_proposer/3, start_acceptor/1, start_learner/1, test1/0,testa/0, testb/0]).

%%%%%%%%%%%%%%%%% Proposer %%%%%%%%%%%%%%%%%%%%%%%%%%%

%TODO: Start proposer with a given list of acceptors (and learners?) OR wait for a message including this list
start_proposer(Value,R,Acceptors) ->
	%Init r=1, r_latest = 0, latest=none, ack_num=0
	propose(Value, R, 0, none, Acceptors).

propose(Value, R, R_latest, Latest, Acceptors) ->
	io:format("~p: Proposer started~n",[self()]),
	lists:foreach(fun(Acceptor) -> Acceptor ! {prepare, self(), R} end, Acceptors),
	proposer_get_acks(Value, R, R_latest, Latest, 0, Acceptors), %ack_num = 0
	nothing.

proposer_get_acks(Value, R, R_latest, Latest, Ack_num, Acceptors) -> 
	io:format("Ack_num: ~w~n", [Ack_num]),
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
					io:format("R_i:~w, R_latest:~w~n", [R_i, R_latest]),
					case (R_i > R_latest) of
						true ->
							io:format("Proposer got ACK~n"),
							proposer_get_acks(Value, R, R_i, Value_i, Ack_num + 1, Acceptors);
						false ->
							proposer_get_acks(Value, R, R_latest, Latest, Ack_num + 1, Acceptors)
					end;
				false ->
					proposer_get_acks(Value, R, R_latest, Latest, Ack_num, Acceptors)
			end
	end.

proposer_accept(Value, R, Acceptors) ->
	io:format("Proposer accept~n"),
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
							%FIXME: Needs NACK or timeout in proposer
							acceptor(R_ack, R_accepted, Value, Learners) 
					end;
				{accept, R, W} -> %Phase 2
					io:format("Got accept message~n"),
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
	%    sleep(1000),
	io:format("Learners: ~w, R_accepted: ~w~n", [Learners, R_accepted]),
	lists:foreach(fun(Learner) -> Learner ! {accepted, R_accepted, W} end, Learners).

%%%%%%%%%%%%%%%%% Learner %%%%%%%%%%%%%%%%%%%%%%%%%%%
start_learner(Maj) ->
	%Init num_acceptors = 0, R = 0, Value = none
	learner(0, 0, none, Maj).

learner(Num_accepted, R_lcl, V_lcl, Maj) ->
	io:format("Learner: Num_accepted=~w Maj=~w~n", [Num_accepted, Maj]),

	%case (Num_accepted == Maj) of
	%    true ->
	%       io:format("Value accepted: ~w~n", V_lcl);
	%    false ->
	%        nothing
	%end,

	if 
		(Num_accepted >= Maj) ->
			io:format("Value accepted: ~w~n", [R_lcl]);
		(Num_accepted < Maj) ->
			io:format("Value not accepted: ~p~n",[R_lcl])
	end,

	io:format("Learner is waiting for accepted msg~n"),

	receive
		Msg ->
			case Msg of
				{accepted, R, Value} ->
					io:format("Learner received accepted msg:~p:~p~n",[R,Value]),
					if
						(R > R_lcl) ->
							io:format("New value"),
							learner(1, R, Value, Maj);
						(R =< R_lcl) ->
							learner(Num_accepted + 1, R_lcl, Value, Maj)
					end; 
				%                casei R > R_lcl of
				%                    true ->
				%                        learner(1, R, Value, Maj);
				%                    false -> %Must be R_lcl == R and V_lcl == Value
				%                        learner(Num_accepted + 1, R_lcl, Value, Maj)
				%                end;
				_ ->
					io:format("WTF?~n")
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
	Proposer = spawn(paxos, start_proposer, [23, 1, [Acceptor2]]),
	Proposer2 = spawn(paxos, start_proposer, [24, 2, [Acceptor1, Acceptor3]]),
	nothing.

testb() ->
	%3 acceptors, 1 learner, 1 proposer, no crash
	Learner = spawn(paxos, start_learner, [3]),
	Acceptor1 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor2 = spawn(paxos, start_acceptor, [[Learner]]),
	Acceptor3 = spawn(paxos, start_acceptor, [[Learner]]),
	Proposer = spawn(paxos, start_proposer, [23, 1, [Acceptor2, Acceptor3]]),
	nothing.

%%%%%%%%%%%%%%%%% Utility funs %%%%%%%%%%%%%%%%%%%%%%%%%%%
sleep(Ms) ->
	receive
	after Ms ->
			io:format("slept ~p ms~n",[Ms]),
			nothing
	end.

