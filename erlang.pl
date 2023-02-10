%  Load with ['erlang.pl'].
% try:
% trace.
% nodebug.
% notrace.

% try:
% call(N is 1 + 2).

% our solve.
solve((A,B)) :- solve(A), solve(B).

solve(A) :- builtin(A), call(A).
solve(A) :- rule(A,B), solve(B).

% Reduce. Better with goals
reduce([]).
reduce([{Native} | T]) :-
    call(Native),
    !,
    reduce(T).
reduce([ Lhs | More ]) :-
    rule(Lhs,Expansion),
    append(Expansion, More, More1), % Add the expansion to the More list, turn into a More1 list.
    !,
    reduce(More1).

% Program the factorial!
rule(fact(0,1), []).
% run trace. then reduce([fact(0,X)]).
rule(fact(N,F), [{ N1 is N -1 }, fact(N1,F1), { F is N * F1 }]).


