/* 

Let's redefine natural number arithmetic in swi prolog.





















 */

% A few words on unification:
/*
  do:
   a = 1.
   A = 1.
   a = B.
   A = B.
   bla(A) = bla(1).
   bla(A) = bla(B).
   [ H | T ] = [ 1 ,2 ,3 , 4].
   bla(A) = bla(baz(B)).
   bla([ H | T ]) = bla([baz(B)]).
   bla(A = 1) = bla(2 = 1).
   =(A,1).

*/

nat(z). % nat/1 is a predicate. No goals. so it's a fact.
nat(s(X)) :- nat(X). % This is a rule. Made of 'predicate(<shape>) :- goals..' aka Horn clauses.
% Note that shape can be any correctly parenthesis expression.
% attach meaning to syntactic shapes . 
sum(X,z,X).
sum(z,X,X).
sum(s(X),Y,s(Z)) :- sum(X,Y,Z).

sumi(X + z = X).
sumi(z + X = X).
sumi(s(X) + Y = s(Z)) :- sumi(X + Y = Z).

mul(_,z,z).
mul(z,_,z).
mul(X,s(z),X).
mul(s(z),Y,Y).
mul(s(X),Y,Z) :- mul(X,Y,ZZ), sum(ZZ,Y,Z).

% trace.
