/* 

Let's redefine natural number arithmetic in swi prolog.





















 */
nat(z).
nat(s(X)) :- nat(X).
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
