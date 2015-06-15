#!bin/estap

-plan(3).

-test("test with sub-plan in advance (some subtests will fail)").
plan_test() ->
  estap:plan(10),
  estap:ok(true, "hardcoded success"),
  estap:is(true, false, "hardcoded failure"),
  estap:isnt(true, false, "another success"),
  estap:eq({1.0, 2.0}, {1, 2}, "cmp (==)"),
  estap:ne({1.0, 2.1}, {1, 2}, "cmp (/=)"),
  estap:cmp("foo", '=<', "bar", "=< (not really)"),
  estap:like("foo", "^f", "regexp match"),
  estap:unlike("foo", "^[A-Z]+$", "regexp negated match"),
  estap:matches("foo", fun("foo") -> ok end, "match test (success)"),
  estap:matches("bar", fun("foo") -> ok end, "match test (failure)"),
  estap:all_ok().

-test("test with sub-plan in advance (some subtests will fail)").
no_plan_test() ->
  estap:ok(true, "hardcoded success"),
  estap:is(true, false, "hardcoded failure"),
  estap:all_ok().

-test("test with all successes").
all_successes() ->
  estap:ok(ok, "first"),
  estap:ok({ok, nabla}, "second"),
  estap:ok(true, "third"),
  estap:all_ok().

%% vim:ft=erlang
