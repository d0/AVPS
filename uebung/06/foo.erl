-module(foo).
-export([f/0,h/2,fac/1]).

f() -> 
    27.

g(X) ->
    f() + X.

h(X,Y) ->
    Z = g(Y),
    X + Z.

fac(0) -> 0;
fac(1) -> 1;
fac(N) -> N * fac(N-1).
