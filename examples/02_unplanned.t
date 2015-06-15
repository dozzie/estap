#!bin/estap

-test("first: success").
first() ->
  ok.

-test("second: failure").
second() ->
  {error, nabla}.

-test("third: success").
third() ->
  true.

-test("fourth: success (throw)").
fourth() ->
  throw(ok).

-test("fifth: failure (error)").
fifth() ->
  A = 1,
  B = two,
  A = B.

%% vim:ft=erlang
