#!bin/estap

-module(qwerty).

-plan(3).

-some_attribute(attr_value).
-another_attribute("string value").

-test("list of module attributes").
attrs() ->
  Attrs = [{N, estap:explain(V)} || {N,V} <- module_info(attributes)],
  estap:info("module attrs", Attrs),
  ok.

-test("process dictionary").
proc_dict() ->
  Dict = [{N, estap:explain(V)} || {N,V} <- get()],
  estap:info("process dictionary", Dict),
  ok.

-test("listing test files").
ls() ->
  estap:info("all test files", filelib:wildcard("*.t", estap:test_dir())),
  ok.

%% vim:ft=erlang
