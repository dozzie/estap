#!bin/estap

-plan(5).

-test("info simple").
info() ->
  estap:plan(2),
  estap:ok(true, "one pass"),
  estap:info("this is an informational message"),
  estap:ok(true, "two pass"),
  estap:all_ok().

-test("warning simple").
warning() ->
  estap:plan(2),
  estap:ok(true, "one pass"),
  estap:diag("this is a warning message"),
  estap:ok(true, "two pass"),
  estap:all_ok().

-test("info with context").
info_2() ->
  estap:plan(2),
  estap:ok(true, "one pass"),
  estap:info("info", [{atom, second_atom}, {"string", "second string"}]),
  estap:info("info", ["additional context (should be indented)"]),
  estap:ok(true, "two pass"),
  estap:all_ok().

-test("warning with context").
warning_2() ->
  estap:plan(2),
  estap:ok(true, "one pass"),
  estap:diag("warn", [{atom, second_atom}, {"string", "second string"}]),
  estap:diag("warn", ["additional context (should be indented)"]),
  estap:ok(true, "two pass"),
  estap:all_ok().

-test("explain").
explain() ->
  estap:plan(2),
  estap:ok(true, "one pass"),
  estap:info("info", [
    {one, estap:explain(dict:new())},
    {two, estap:explain(sets:new())}
  ]),
  estap:ok(true, "two pass"),
  estap:all_ok().

%% vim:ft=erlang
