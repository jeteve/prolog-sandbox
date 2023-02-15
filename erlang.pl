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
% fact1(N) -> .. _assign_  Result.. return(Result)
% Let's write an interpreter that allows assignments and returning.
%
%
