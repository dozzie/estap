%%%---------------------------------------------------------------------------
%%% @doc
%%%   Input/output and test plan tracking process.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap_server).

-behaviour(gen_server).

%% public interface
-export([no_plan/0, plan/1, subplan/2, done/1]).
-export([running/2, report_result/2, report_result_todo/3, report_skipped/2]).

%% supervision tree API
-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-export_type([test_run_id/0]).

%%%---------------------------------------------------------------------------

-type test_run_id() :: pid().

-type plan() :: no_plan | pos_integer().

-record(state, {
  plan :: plan(),
  level = 0 :: non_neg_integer(),
  test :: {TestNo :: pos_integer(), Description :: string()}
}).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc "No plan" plan.

-spec no_plan() ->
  test_run_id().

no_plan() ->
  {ok, Pid} = start_link(no_plan, 0),
  Pid.

%% @doc Test plan.

-spec plan(pos_integer()) ->
  test_run_id().

plan(TestCount) when is_integer(TestCount) ->
  {ok, Pid} = start_link(TestCount, 0),
  Pid.

%% @doc Plan for subtests.

-spec subplan(plan(), pos_integer()) ->
  test_run_id().

subplan(Plan, Level)
when Plan == no_plan orelse is_integer(Plan), is_integer(Level) ->
  {ok, Pid} = start_link(Plan, Level),
  Pid.

%% @doc Mark the end of tests in this run.

-spec done(test_run_id()) ->
  ok.

done(TestRunId) ->
  gen_server:call(TestRunId, done).

%% @doc Mark the beginning of new test.

-spec running(test_run_id(), string()) ->
  ok.

running(TestRunId, Description) ->
  gen_server:call(TestRunId, {next, Description}).

%% @doc Report result of running a test started at {@link running/2}.

-spec report_result(test_run_id(),
                    {success | failure | dubious | died, term()}) ->
  ok.

report_result(TestRunId, {success, _Value} = _TestResult) ->
  gen_server:call(TestRunId, {result, success});

report_result(TestRunId, {failure, ReturnValue} = _TestResult) ->
  gen_server:call(TestRunId, {result, {failure, ReturnValue}});

report_result(TestRunId, {dubious, ReturnValue} = _TestResult) ->
  gen_server:call(TestRunId, {result, {dubious, ReturnValue}});

report_result(TestRunId, {died, Reason} = _TestResult) ->
  gen_server:call(TestRunId, {result, {died, Reason}}).

%% @doc Report result of running a TODO test started at {@link running/2}.

-spec report_result_todo(test_run_id(), string(),
                         {success | failure | dubious | died, term()}) ->
  ok.

report_result_todo(TestRunId, Why, {success, _Value} = _TestResult) ->
  gen_server:call(TestRunId, {todo, success, Why});

report_result_todo(TestRunId, Why, {failure, ReturnValue} = _TestResult) ->
  gen_server:call(TestRunId, {todo, {failure, ReturnValue}, Why});

report_result_todo(TestRunId, Why, {dubious, ReturnValue} = _TestResult) ->
  gen_server:call(TestRunId, {todo, {dubious, ReturnValue}, Why});

report_result_todo(TestRunId, Why, {died, Reason} = _TestResult) ->
  gen_server:call(TestRunId, {todo, {died, Reason}, Why}).

%% @doc Report that a test started at {@link running/2} was skipped.

-spec report_skipped(test_run_id(), string()) ->
  ok.

report_skipped(TestRunId, Why) ->
  gen_server:call(TestRunId, {skipped, Why}).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the process.

-spec start(plan(), non_neg_integer()) ->
  {ok, pid()} | {error, term()}.

start(Plan, _Level) ->
  gen_server:start(?MODULE, [Plan], []).

%% @private
%% @doc Start the process.

-spec start_link(plan(), non_neg_integer()) ->
  {ok, pid()} | {error, term()}.

start_link(Plan, _Level) ->
  gen_server:start_link(?MODULE, [Plan], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize event handler.

init([Plan] = _Args) ->
  case Plan of
    no_plan -> skip;
    C when is_integer(C) -> io:fwrite("1..~B~n", [C])
  end,
  State = #state{plan = Plan},
  {ok, State}.

%% @private
%% @doc Clean up after event handler.

terminate(_Arg, _State) ->
  ok.

%% }}}
%%----------------------------------------------------------
%% communication {{{

%% @private
%% @doc Handle {@link gen_server:call/2}.

handle_call(done = _Request, _From,
            State = #state{plan = Plan, test = {LastTestNo, _}}) ->
  case Plan of
    no_plan -> io:fwrite("1..~B~n", [LastTestNo]);
    C when is_integer(C) -> io:fwrite("# ran ~B of ~B tests~n", [LastTestNo, C])
  end,
  {stop, normal, ok, State};

handle_call({next, Desc} = _Request, _From,
            State = #state{test = Test}) ->
  NextTestNo = case Test of
    undefined -> 1;
    {TestNo, _} -> TestNo + 1
  end,
  NewState = State#state{test = {NextTestNo, Desc}},
  {reply, ok, NewState};

handle_call({result, TestResult} = _Request, _From,
            State = #state{test = {TestNo, TestDesc}}) ->
  case TestResult of
    success ->
      io:fwrite("ok ~B - ~s~n", [TestNo, TestDesc]);
    {failure, Reason} ->
      io:fwrite("not ok ~B - ~s # result: ~s~n",
                [TestNo, TestDesc, format(Reason)]);
    {dubious, Value} ->
      io:fwrite("not ok ~B - ~s # dubious result: ~s~n",
                [TestNo, TestDesc, format(Value)]);
    {died, Reason} ->
      io:fwrite("not ok ~B - ~s # died: ~s~n",
                [TestNo, TestDesc, format(Reason)])
  end,
  {reply, ok, State};

handle_call({todo, TestResult, Why} = _Request, _From,
            State = #state{test = {TestNo, TestDesc}}) ->
  case TestResult of
    success ->
      io:fwrite("ok ~B - ~s # TODO ~s~n", [TestNo, TestDesc, Why]);
    {failure, _Reason} ->
      io:fwrite("not ok ~B - ~s # TODO ~s~n", [TestNo, TestDesc, Why]);
    {dubious, _Value} ->
      io:fwrite("not ok ~B - ~s # TODO ~s~n", [TestNo, TestDesc, Why]);
    {died, _Reason} ->
      io:fwrite("not ok ~B - ~s # TODO ~s~n", [TestNo, TestDesc, Why])
  end,
  {reply, ok, State};

handle_call({skipped, Reason} = _Request, _From,
            State = #state{test = {TestNo, TestDesc}}) ->
  io:fwrite("ok ~B - ~s # SKIP ~s~n", [TestNo, TestDesc, Reason]),
  {reply, ok, State};

%% unknown calls
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
%% @doc Handle {@link gen_server:cast/2}.

%% unknown casts
handle_cast(_Request, State) ->
  {noreply, State}.

%% @private
%% @doc Handle incoming messages.

%% unknown messages
handle_info(_Message, State) ->
  {noreply, State}.

%% }}}
%%----------------------------------------------------------
%% code change {{{

%% @private
%% @doc Handle code change.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% }}}
%%----------------------------------------------------------

%% @doc Format term for printing to screen.

-spec format(term()) ->
  iolist().

format(Term) ->
  % no term should weigh 1MB
  io_lib:print(Term, 1, 1024 * 1024, -1).

%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
