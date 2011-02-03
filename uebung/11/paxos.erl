%%2011 Dominik Oepen, Jan Birkholz
-module(paxos).
-export([start_proposer/1, start_acceptor/0, start_learner/0]).

%%%%%%%%%%%%%%%%% Proposer %%%%%%%%%%%%%%%%%%%%%%%%%%%

%TODO: Start proposer with a given list of acceptors (and learners?) OR wait for a message including this list
start_proposer(Value) ->
    %Init r=1, r_latest = 0, latest=none, ack_num=0
    propose(Value, 1, 0, none).

propose(Value, R, R_latest, Latest) ->
    %TODO: Needs list of Acceptors
    lists:foreach(fun(Acceptor) -> Acceptor ! {prepare, self(),  R} end, Acceptors),
    proposer_get_acks(Value, R, R_latest, Latest, 0). %ack_num = 0
    
proposer_get_acks(Value, R, R_latest, Latest, Ack_num) -> 
    case length(Acceptors) > Ack_num of
        true ->
            case Latest of
                none -> %Propose own value
                    proposer_accept(Value, R);
                _ ->
                    proposer_accept(Latest, R)
            end;

        false ->
            nothing.
    end

    receive {ack, R_ack, Value_i, R_i} ->
        case (R == R_ack) of
            true ->
                case (R_i > R_latest) of
                    true ->
                        proposer_get_acks(Value, R, R_i, Value_i, Ack_num + 1);
                    false ->
                        nothing
                end;
            false ->
                nothing
        end
    end.

proposer_accept(Value, R) ->
    %TODO: Needs list of Acceptors
    lists:foreach(fun(Acceptor) -> Acceptor ! {accept, Value, R} end, Acceptors).

%%%%%%%%%%%%%%%%% Acceptor %%%%%%%%%%%%%%%%%%%%%%%%%%%
start_acceptor() ->
    %init R_ack = 0, R_accepted = 0, Value = none
    acceptor(0, 0, none).

acceptor(R_ack, R_accepted, Value) ->
    receive
        Msg ->
        case Msg of
            {prepare, Pid, R} -> %Phase 1
                case (R > R_ack) and (R > R_accepted) of
                    true ->
                        Pid ! {ack, R, Value, R_accepted),
                        acceptor(R, R_accepted, Value);
                    false ->
                        nothing
                end;
            {accept, R, W} -> %Phase 2
                case (R > R_ack) and (R > R_accepted) of
                    true ->
                        acceptor_accepted(R, W),
                        acceptor(R_ack, R, W);
                    false ->
                        acceptor(R_ack, R_accepted, Value)
                end
        end
    end.

acceptor_accepted(R_accepted, W) ->
    %TODO: Needs list of Learners
    lists:foreach(fun(Learner) -> Learner ! {R_accepted, W} end, Learners).

%%%%%%%%%%%%%%%%% Learner %%%%%%%%%%%%%%%%%%%%%%%%%%%
start_learner() ->
    %Init num_acceptors = 0, R = 0, Value = none
    learner(0, 0, none).

learner(Num_accepted, R_lcl, V_lcl) ->
    case Num_accepted == length(Acceptors) of %TODO: Needs list of acceptors
        true ->
           io:format("Value accepted: ~w~n", Value);
        false ->
            nothing
    end

    receive {accepted, R, Value} ->
        case R > R_lcl of
            true ->
                learner(1, R, Value)
            false -> %Must be R_lcl == R and V_lcl == Value
                learner(Num_accepted + 1, R_lcl, Value)
         end
    end.
