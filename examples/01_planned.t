#!bin/estap

-plan(4).

-test("first: success").
first() ->
  ok.

-test("second: success").
second() ->
  ok.

-test("third: failure").
third() ->
  error.

-test("fourth: success").
fourth() ->
  ok.

%% vim:ft=erlang
