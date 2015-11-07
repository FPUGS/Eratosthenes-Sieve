-module(sieve).
-export([sieve/1, segmentedSieve/1]).

sieve(N) when N < 2 -> undefined;
sieve(N) ->
  A = array:new(N + 1, {default, true}),
  array:foldr(fun (I, V, R) when I > 1 andalso V -> [I | R]; (_, _, R) -> R end, [], doSieve(2, trunc(math:sqrt(N)), A)).

doSieve(I, Max, A) when I =< Max ->
  case array:get(I, A) of
    true ->
      doSieve(I + 1, Max, markNonPrimes(I * I, array:size(A), I, A));
    false ->
      doSieve(I + 1, Max, A)
  end;
doSieve(_, _, A) -> A.

markNonPrimes(J, Max, Step, A) when J < Max ->
  markNonPrimes(J + Step, Max, Step, array:set(J, false, A));
markNonPrimes(_, _, _, A) -> A.

segmentedSieve(N) when N < 2 -> undefined;
segmentedSieve(2) -> [2];
segmentedSieve(3) -> [2, 3];
segmentedSieve(N) ->
  Delta = trunc(math:sqrt(N)),
  % TODO
  [].

