%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions to use when running test cases.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap_test).

%% public interface
-export([run/2]).
-export([success_or_failure/1]).

%% private interface
-export([call/5]).

-export_type([test/0, test_plan/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type test() :: {Func :: {module(), atom()}, Description :: string(),
                  Status :: run | {todo | skip, Why :: string()}}.

-type test_plan() :: {plan, pos_integer()} | no_plan.

%%% }}}
%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------

%% @doc Run tests according to plan.

-spec run(test_plan(), [test()]) ->
  ok.

run(Plan, Tests) ->
  TestRun = case Plan of
    no_plan   -> estap_server:no_plan();
    {plan, C} -> estap_server:plan(C)
  end,
  try
    run_tests(TestRun, Tests),
    estap_server:done(TestRun)
  catch
    throw:'BAIL_OUT' ->
      ok
  end.

%% @doc Run tests, one by one, reporting their results to tracking process.
%%
%% @TODO Return something meaningful.

-spec run_tests(estap_server:test_run_id(), [test()]) ->
  ok.

run_tests(_TestRun, [] = _Tests) ->
  ok;
run_tests(TestRun, [{TestFunSpec, Description, Status} | Rest] = _Tests) ->
  estap_server:running(TestRun, Description),
  case Status of
    run ->
      Result = test(TestRun, TestFunSpec),
      estap_server:report_result(TestRun, Result);
    {todo, Why} ->
      Result = test(TestRun, TestFunSpec),
      estap_server:report_result_todo(TestRun, Why, Result);
    {skip, Why} ->
      estap_server:report_skipped(TestRun, Why)
  end,
  run_tests(TestRun, Rest).

%% @doc Run a single test function, according to its specification.

-spec test(estap_server:test_run_id(),
           {Module :: module(), Function :: atom()}) ->
    {success, term()}
  | {failure, term()}
  | {dubious, term()}
  | {died, term()}.

test(TestRun, {Mod, Func} = _TestFunSpec) ->
  Args = [],
  ResultRef = make_ref(),
  ResultTo = {self(), ResultRef},
  {Pid, MonRef} = spawn_monitor(?MODULE, call, [ResultTo, TestRun, Mod, Func, Args]),
  receive
    {result, ResultRef, TestResult} ->
      erlang:demonitor(MonRef, [flush]),
      TestResult;
    {'DOWN', MonRef, process, Pid, 'BAIL_OUT'} ->
      throw('BAIL_OUT');
    {'DOWN', MonRef, process, Pid, Reason} ->
      {died, Reason}
  end.

%% @private
%% @doc Run the specified function, collect its result (possibly thrown) and
%%   report it back to `ResultTo'.

-spec call({pid(), term()}, estap_server:test_run_id(),
           module(), atom(), [term()]) ->
  ok.

call({Pid, Ref} = _ResultTo, TestRun, Mod, Fun, Args) ->
  % XXX: for `estap:info()' and `estap:diag()' to work with no sub-tests
  put(estap_server_parent, TestRun),
  % XXX: putting `test_dir' to proc dict is an important thing for
  % `estap:test_dir()' function
  ModuleAttrs = Mod:module_info(attributes),
  case proplists:get_value(test_dir, ModuleAttrs) of
    [DirName] -> put(test_dir, DirName);
    DirName when is_list(DirName) -> put(test_dir, DirName);
    _ -> ok
  end,
  TestResult = try
    success_or_failure(apply(Mod, Fun, Args))
  catch
    throw:ok          -> {success, ok};
    throw:{ok, Value} -> {success, {ok, Value}};
    throw:true        -> {success, true};
    throw:error           -> {failure, error};
    throw:{error, Reason} -> {failure, {error, Reason}};
    throw:false           -> {failure, false}
  end,
  Pid ! {result, Ref, TestResult},
  ok.

%%----------------------------------------------------------

%% @doc Assess whether the value (returned by some function) is a success or
%%   failure.

-spec success_or_failure(Value) ->
    {success, Value}
  | {failure, Value}
  | {dubious, Value}.

success_or_failure(ok = Value)      -> {success, Value};
success_or_failure({ok, _} = Value) -> {success, Value};
success_or_failure(true = Value)    -> {success, Value};
success_or_failure(error = Value)      -> {failure, Value};
success_or_failure({error, _} = Value) -> {failure, Value};
success_or_failure(false = Value)      -> {failure, Value};
success_or_failure(Value) -> {dubious, Value}.

%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
