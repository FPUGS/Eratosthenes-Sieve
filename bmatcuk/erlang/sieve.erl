-module(sieve).
-export([sieve/1, segmentedSieve/1, mySieve/1, divisionSieve/1, test/0]).

% A straight implementation of the Sieve of Eratosthenes
sieve(N) when N < 2 -> undefined;
sieve(N) ->
  A = array:new(N + 1, {default, true}),
  array:foldr(fun (I, V, R) when I > 1 andalso V -> [I | R]; (_, _, R) -> R end, [], doSieve(2, round(math:sqrt(N)), A)).

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



% A segmented sieve - this is actually faster than the normal sieve because of
% the odd performance characteristics of erlang "arrays". Arrays do not have
% true random access - they're implemented as a list of tuples. Since this
% operates on smaller arrays, less time is wasted traversing the array.
segmentedSieve(N) when N < 2 -> undefined;
segmentedSieve(2) -> [2];
segmentedSieve(3) -> [2, 3];
segmentedSieve(N) ->
  Delta = round(math:sqrt(N)),
  Initial = sieve(Delta),
  DoMarks = fun DoMarks(Min, Max, [P | Marks], A) -> DoMarks(Min, Max, Marks, markNonPrimes(case Min rem P of 0 -> 0; S -> P - S end, Max - Min + 1, P, A));
                DoMarks(_, _, _, A) -> A end,
  DoSegment = fun DoSegment(Min, Max, Primes) ->
                  A = array:new(Max - Min + 1, {default, true}),
                  NewPrimes = Primes ++ array:foldr(fun (I, V, R) when V -> [I + Min | R]; (_, _, R) -> R end, [], DoMarks(Min, Max, Initial, A)),
                  case Max of
                    N -> NewPrimes;
                    _ -> DoSegment(Min + Delta, min(Max + Delta, N), NewPrimes)
                  end
              end,
  DoSegment(Delta + 1, Delta + Delta, Initial).



% A more "erlang" implementation that is, unfortunately, a bit slower =(
mySieve(N) when N < 2 -> undefined;
mySieve(N) ->
  Max = round(math:sqrt(N)),
  DoMySieve = fun DoMySieve(Primes, [I | List]) when I =< Max ->
                    DoMySieve([I | Primes], List -- lists:seq(I * I, N, I + I));
                  DoMySieve(Primes, List) -> lists:reverse(Primes, List) end,
  [2 | DoMySieve([], lists:seq(3, N, 2))].



% This is a very simple implementation that relies on division (rem) to filter
% out the multiples of primes.
divisionSieve(N) when N < 2 -> undefined;
divisionSieve(N) ->
  DoSieve = fun DoSieve([H | T]) -> [H | DoSieve(lists:filter(fun(I) -> I rem H /= 0 end, T))];
                DoSieve(_) -> []
            end,
  DoSieve(lists:seq(2, N)).



% Try them out
test() ->
  Run = fun (Algo) ->
            {Time, _} = timer:tc(?MODULE, Algo, [10000]),
            io:fwrite("~w executed in ~wus~n", [Algo, Time])
        end,
  Run(sieve),
  Run(segmentedSieve),
  Run(mySieve),
  Run(divisionSieve).

