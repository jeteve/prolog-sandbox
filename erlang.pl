%  Load with ['erlang.pl'].
% try:
% trace.
% nodebug.
% notrace.

% try:
% call(N is 1 + 2).

% A naive solve.
solve((A,B)) :- solve(A), solve(B).
solve(A) :- builtin(A), call(A).
solve(A) :- rule(A,B), solve(B).

%
% Concurrency
%
% First goal: An interruptable interpreter.
% capable of interleaving computations.
%

% Reduce. Better with goals
reduce([]).
reduce([{Native} | Tail]) :-
    call(Native), % call is a prolog thing.
    !,
    reduce(Tail).
reduce([ Lhs | More ]) :-
    rule(Lhs,Expansion),
    append(Expansion, More, More1), % Add the expansion to the More list, turn into a More1 list.
    !,
    reduce(More1).

% Program the factorial!
rule(fact(0,1), []).
% run trace. then reduce([fact(0,X)]).
rule(fact(N,F), [{ N1 is N -1 }, fact(N1,F1), { F is N * F1 }]).
% then run reduce([fact(2,X)]).
% or to write the result:
% reduce([fact(3,F), {write(result(F)), nl}]).

rule(fibo(0,1), []).
rule(fibo(1,1), []).
rule(fibo(N,R), [{ N1 is N - 1}, { N2 is N - 2}, fibo(N1, R1), fibo(N2, R2), { R is R1 + R2 }]).

%%
% Next step: Suspend execution after N operations.
%%
% No more goals left. Terminated in N operations:
reduce([], Nops, terminated(Nops)):- !.
% Max 4 reductions. Resolves to a continuation of the same goals.
reduce(Goals,4, continuation(Goals)):- !.

% Then the standard implementation with the rules and stuff:
% Note how result is kept the same for the whole of the
% computation.
reduce([{Native} | Tail], Nops, Result):-
    call(Native),!,
    Nops1 is Nops + 1,
    reduce(Tail, Nops1, Result).

reduce([H | T], Nops, Result):-
    rule(H, Expansion),
    append(Expansion, T , T1),
    Nops1 is Nops + 1,
    reduce(T1, Nops1, Result).

% Meh..
% reduce(continuation(Gs)):- reduce(Gs,0,R),!, write(R).

% Try:
% reduce([fact(1,X)],0,R), write(R), !.
% reduce([fact(2,X)],0,R), write(R).
% then continue..


%
% Continue: Multi reduce several goals in a round robin fashion.
% 4 goals at a time.
%
%
multi_reduce([]).
% one job and the other jobs
multi_reduce([job(Jid, Goals) | Otherjobs ]) :-
    write(starting(Jid)),nl,
    reduce(Goals,0, Reduction), % <-- Note the use of reduce/3 that has at most 4 computation steps.
    multi_reduce(Reduction, Jid, Otherjobs). % Reduction can be terminated(Something) or continuation(Goals).
    % ^^ so we need a multi_reduce/3.

% In terminated is the number of steps. Not very interesting.
multi_reduce(terminated(_), Jid, Otherjobs) :-
    write(termination(Jid)), nl,
    multi_reduce(Otherjobs). % <-- And continue the rest of the jobs.

multi_reduce(continuation(Goals), Jid, Otherjobs) :-
    write(suspension(Jid)), nl,
    append(Otherjobs, [job(Jid, Goals)], ExtendedOthers), % <-- Put the 'continuation' job at the end of the queue
    multi_reduce(ExtendedOthers). % <-- And start munching the job queue with the next job first (hence round robin)


% Then try:
/*

multi_reduce([job(1,[fact(1,N)]), job(2,[fact(2,N2)])]).

multi_reduce([job(1,[fact(1,N)]), job(2,[fact(3,N2), {write(N2),nl}])]).

multi_reduce([job(1,[fact(5,N)]), job(2,[fibo(6,N2), {write(N2),nl}])]).
*/

% What would happen if N2 was turned into N?


% Ok, we have round robin scheduling!
% notice the uglyness of 'rule'? What about we can define functions like:
% fact1(0) -> return(1);
% fact1(N) -> .. _assign recursive calls_  Result.. return(Result)
% And call them like:
% X = f(Y). whe f finishes with return(Something).
% instead of P(X,Y)
% Result = function(Arg), write(Result).
% Let's write an interpreter that allows assignments and returning.
%
%

reducef([]).
reducef([ Var = Rhs | T]) :- !,
    reducef([ Rhs, '$bind'(Var) | T]). % <-- $bind is a special token. Not to be used by anything else. We'll reduce it later
% ^^ we will reduce the Rhs and we carry the $bind(Var) with us for the rest of the reduction.

% Something that returns ALWAYS have to return into something. In other words, return and = are both sides of the same
% feature.
reducef([return(Value), '$bind'(Var) | T]) :- !,
    =(Var, Value), % Unify. Value is now bounded to the same as Var
    reducef(T). % Q: Why is $bind 

% Case of native predicate
reducef([{X} | T ]):-
    call(X), !,
    reducef(T).

% Start building in some functions native in our language instead of
% using the native prolog ones:
reducef([write(X) | T]) :- !,
    write(X),
    reducef(T).

% same for nl
reducef([nl | T]) :- !,
    nl,
    reducef(T).

reducef([H | T]) :-
    deff(H, Body), ! , % Note we call that deff (as function definition :))
    append(Body, T , T1), % we expanded the body.
    reducef(T1).


deff(addone(0), [
    return(1)
]).
deff(addone(X), [
    { R is X + 1 },
    return(R)
]).

% Try: reducef([X = addone(1), write(X), nl ]).
% Try: reducef([addone(5)]).
% Note: nothing to resolve. False.

% Try trace and see how the thing is resolved 

:- discontiguous deff/2.
deff(fact(0), [ return(1) ]).
deff(fact(N), [{ N1 is N -1 }, F1 = fact(N1), { R is N * F1 }, return(R)]).

% Try: 
% reducef([X = fact(256), write(X), nl ]).

% Putting it all together 
% so . Functional + Messaging between processes + Suspending (aka waiting for messages).
% reducem = reducemessages.
% reducem(PGoals, Pid, PMailbox, Ps).
% PGoals, Pid, PMailbox: Current process properties.
% Ps: List of processes. (AKA jobs). Because I'm lazy, I'll be talking about jobs.

% No current goals left, no jobs left. Pid and PMailbox are irrelevant.
reducem([], _, _, [] ):- !, % <-- No point backtracking.
    write(world_stopped), nl.

% No goals left in current job, Need to get the next job up
% note we forget about his job's mailbox.
reducem([], Pid, _ , [job(Id, Goals, Msgs)| T]):- !,
    write(terminated(Pid)), nl,
    write(resuming(Id)), nl,
    reducem(Goals,Id,Msgs,T).

% Implement the native 'spawn' function to spawn a new job with goals.
reducem([spawn(Id,Goals)| T], Myid, Mymsgs, Otherjobs):- !,
    write(spawning(Id)), nl,
    append(Otherjobs, [job(Id,Goals,[])], Morejobs),
    reducem(T,Myid,Mymsgs,Morejobs). % <-- New job is queued, move on to the next current goals.

% Implement the native 'send' function to send a message to a job ID
reducem([send(Id,Msg) | T], Myid, Mymsgs, Otherjobs):-
    send(Id,Msg,Otherjobs, Changedjobs), % <-- Sending a message will create a new set of other jobs.
    !,
    reducem(T, Myid, Mymsgs , Changedjobs). % <-- Reducing the next goals with the new environment.
    
% Implement the native blocking (if no message) receive function
% There is at least one Msg in my inbox
reducem([ receive | T], Myid, [Msg | More], Otherjobs):- !,
    reducem([return(Msg) | T], Myid, More, Otherjobs). % <-- We assume we're going to do M = receive, so we turn the receive into a return.

% Implement blocking call when no message in the inbox yet.
reducem([receive | T] , Myid, [], Otherjobs):- !,
    write(suspending(Myid)), nl,
    append(Otherjobs, [job(Myid,[receive | T], [])], Morejobs), % <-- Give another chance to receive next time it is schedule.
    reducem([], none, [], Morejobs). % <-- The reduction restarts with the next job in line.

% Now the standard implementation. with the return and the native goal
reducem([ Var = Rhs | T ], Myid, Mymsgs, Otherjobs):- !,
    reducem([ Rhs, '$bind'(Var) | T ], Myid, Mymsgs, Otherjobs).

reducem([ return(Value), '$bind'(Var) | T], Myid, Mymsgs, Otherjobs):- !,
    Var = Value,
    reducem(T, Myid, Mymsgs, Otherjobs). % Note how the assignment and the return go together.

reducem([{Native} | T], Myid , Mymsgs, Otherjobs):-
    call(Native), !,
    reducem(T, Myid, Mymsgs, Otherjobs).

reducem([ Lhs | T], Myid, Mymsgs, Otherjobs):-
    deff(Lhs, Rhs), !,
    append(Rhs, T , T1),
    reducem(T1, Myid, Mymsgs, Otherjobs).


% implement send. Lookup the job with the same ID and add the message to its inbox
send(Id,Msg, [job(Id, Goals, Msgs) | T ],
             [job(Id, Goals, Newmsgs) | T]
            ) :- !,
    append(Msgs, [Msg], Newmsgs). % < -- Put the new message at the end of the new messsages
% the recursive search. We dont touch the head as it doesnt match. We just want to transform
% T into T1 by attempting to send the message again.
send(Id, Msg, [ H | T], [ H | T1 ]):- send(Id, Msg, T , T1).


deff(toplevel, [
        spawn(showfact, [showfact(3)]),
        X = receive,
        write(X),nl
    ]).

deff(showfact(N),[
    X = fact(N),
    { write(X), nl}
]).

% Try: reducem([], start, [],[job(showfact,[showfact(2)],[])]).


deff(go,[
    spawn(sender, [sender(5)]),
    spawn(catcher, [catch])
]).

deff(sender(0),[
    send(catcher, stop)
]).

deff(sender(N),[
    {write(sending(pip(N))),nl},
    send(catcher, pip(N)),
    {N1 is N -1 },
    sender(N1)
]).

deff(catch,[
    X = receive,
    {write(received(X)),nl},
    catch(X)
]).

deff(catch(stop),[]).
deff(catch(_),[catch]). % Catch again


% Try again: reducem([go],startup,[],[]).