%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions to use when running test cases.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap_test).

%% public interface
-export([run/2]).

%% private interface
-export([call/4]).

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
  run_tests(TestRun, Tests),
  estap_server:done(TestRun).

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
      case test(TestFunSpec) of
        {success, _Result} ->
          estap_server:test_passed(TestRun);
        {failure, Result} ->
          estap_server:test_failed(TestRun, Result);
        {dubious, Result} ->
          estap_server:dubious_result(TestRun, Result);
        {died, Reason} ->
          estap_server:test_died(TestRun, Reason)
      end;
    {todo, Why} ->
      %estap_server:test_todo(TestRun, Why)
      case test(TestFunSpec) of
        {success, _Result} ->
          estap_server:test_todo(TestRun, success, Why);
        {failure, Result} ->
          estap_server:test_todo(TestRun, {failure, Result}, Why);
        {dubious, Result} ->
          estap_server:test_todo(TestRun, {dubious, Result}, Why);
        {died, Reason} ->
          estap_server:test_todo(TestRun, {died, Reason}, Why)
      end;
    {skip, Why} ->
      estap_server:test_skipped(TestRun, Why)
  end,
  run_tests(TestRun, Rest).

%% @doc Run a single test function, according to its specification.

-spec test({Module :: module(), Function :: atom()}) ->
    {success, term()}
  | {failure, term()}
  | {dubious, term()}
  | {died, term()}.

test({Mod, Func} = _TestFunSpec) ->
  Args = [],
  ResultRef = make_ref(),
  ResultTo = {self(), ResultRef},
  {Pid, MonRef} = spawn_monitor(?MODULE, call, [ResultTo, Mod, Func, Args]),
  receive
    {result, ResultRef, TestResult} ->
      erlang:demonitor(MonRef, [flush]),
      TestResult;
    {'DOWN', MonRef, process, Pid, Reason} ->
      {died, Reason}
  end.

%% @private
%% @doc Run the specified function, collect its result (possibly thrown) and
%%   report it back to `ResultTo'.

-spec call({pid(), term()}, module(), atom(), [term()]) ->
  ok.

call({Pid, Ref} = _ResultTo, Mod, Fun, Args) ->
  TestResult = try apply(Mod, Fun, Args) of
    ok          -> {success, ok};
    {ok, Value} -> {success, {ok, Value}};
    true        -> {success, true};
    error           -> {failure, error};
    {error, Reason} -> {failure, {error, Reason}};
    false           -> {failure, false};
    Result          -> {dubious, Result}
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

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
