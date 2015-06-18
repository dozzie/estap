%%%---------------------------------------------------------------------------
%%% @doc
%%%   Input/output and test plan tracking process.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap_server).

-behaviour(gen_server).

%% public interface
-export([no_plan/0, plan/1, subplan/2, done/1, bail_out/2]).
-export([get_status/1]).
-export([info/2, warning/2]).
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

-record(counters, {
  tests     = 0,
  successes = 0,
  failures  = 0,
  todo_successes = 0,
  todo_failures  = 0,
  skipped   = 0
}).

-record(state, {
  plan :: plan(),
  level = 0 :: non_neg_integer(),
  test :: {TestNo :: pos_integer(), Description :: string()},
  counters = #counters{}
}).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% test plan setup {{{

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

%% @doc Return summary in form of numbers of tests.
%%   `TODO' tests are the ones marked as TODO that failed.

-spec get_status(test_run_id()) ->
  {Planned :: integer() | undefined,
    Total :: integer(), Failed :: integer(), TODO :: integer()}.

get_status(TestRunId) ->
  gen_server:call(TestRunId, status).

%% }}}
%%----------------------------------------------------------
%% test plan execution {{{

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

%% @doc Abort the test script because of fatal error.

-spec bail_out(test_run_id(), string()) ->
  ok.

bail_out(TestRunId, Reason) ->
  gen_server:call(TestRunId, {bail_out, Reason}).

%% }}}
%%----------------------------------------------------------
%% printing messages {{{

%% @doc Print a regular message to test output.

-spec info(test_run_id(), iolist()) ->
  ok.

info(TestRunId, Message) ->
  gen_server:call(TestRunId, {info, Message}).

%% @doc Print a diagnostic message (warning) to output, typically
%%   <i>STDERR</i>.

-spec warning(test_run_id(), iolist()) ->
  ok.

warning(TestRunId, Message) ->
  gen_server:call(TestRunId, {warning, Message}).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the process.

-spec start(plan(), non_neg_integer()) ->
  {ok, pid()} | {error, term()}.

start(Plan, Level) ->
  gen_server:start(?MODULE, [Plan, Level], []).

%% @private
%% @doc Start the process.

-spec start_link(plan(), non_neg_integer()) ->
  {ok, pid()} | {error, term()}.

start_link(Plan, Level) ->
  gen_server:start_link(?MODULE, [Plan, Level], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize event handler.

init([Plan, Level] = _Args) ->
  State = #state{plan = Plan, level = Level},
  case Plan of
    no_plan -> skip;
    C when is_integer(C) -> print("1..~B", [C], State)
  end,
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
    no_plan ->
      print("1..~B", [LastTestNo], State);
    C when is_integer(C) ->
      print("# ran ~B of ~B tests", [LastTestNo, C], State)
  end,
  {stop, normal, ok, State};

handle_call({bail_out, Reason} = _Request, _From, State) ->
  print("Bail out! ~s", [Reason], State),
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
            State = #state{test = {TestNo, TestDesc}, counters = Counters}) ->
  case TestResult of
    success ->
      print("ok ~B - ~s", [TestNo, TestDesc], State);
    {failure, Reason} ->
      print("not ok ~B - ~s # result: ~s",
            [TestNo, TestDesc, format(Reason)], State);
    {dubious, Value} ->
      print("not ok ~B - ~s # dubious result: ~s",
            [TestNo, TestDesc, format(Value)], State);
    {died, Reason} ->
      print("not ok ~B - ~s # died: ~s",
            [TestNo, TestDesc, format(Reason)], State)
  end,
  NewState = State#state{counters = add_result(TestResult, Counters)},
  {reply, ok, NewState};

handle_call({todo, TestResult, Why} = _Request, _From,
            State = #state{test = {TestNo, TestDesc}, counters = Counters}) ->
  case TestResult of
    success ->
      print("ok ~B - ~s # TODO ~s", [TestNo, TestDesc, Why], State);
    {failure, _Reason} ->
      print("not ok ~B - ~s # TODO ~s", [TestNo, TestDesc, Why], State);
    {dubious, _Value} ->
      print("not ok ~B - ~s # TODO ~s", [TestNo, TestDesc, Why], State);
    {died, _Reason} ->
      print("not ok ~B - ~s # TODO ~s", [TestNo, TestDesc, Why], State)
  end,
  NewState = State#state{counters = add_todo(TestResult, Counters)},
  {reply, ok, NewState};

handle_call({skipped, Reason} = _Request, _From,
            State = #state{test = {TestNo, TestDesc}, counters = Counters}) ->
  print("ok ~B - ~s # SKIP ~s", [TestNo, TestDesc, Reason], State),
  NewState = State#state{counters = add_skipped(Counters)},
  {reply, ok, NewState};

handle_call(status = _Request, _From, State = #state{counters = Counters}) ->
  Planned = case State of
    #state{plan = no_plan} -> undefined;
    #state{plan = C} -> C
  end,
  Total = Counters#counters.tests,
  Failed = Counters#counters.failures,
  TODO = Counters#counters.todo_failures,
  {reply, {Planned, Total, Failed, TODO}, State};

handle_call({info, Message} = _Request, _From, State) ->
  print_info("~s", [Message], State),
  {reply, ok, State};

handle_call({warning, Message} = _Request, _From, State) ->
  print_warning("~s", [Message], State),
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

%%----------------------------------------------------------
%% helper functions
%%----------------------------------------------------------
%% printing messages {{{

%% @doc Format term for printing to screen.

-spec format(term()) ->
  iolist().

format(Term) ->
  % no term should weigh 1MB
  io_lib:print(Term, 1, 1024 * 1024, -1).

%% @doc Print text to screen.
%%   The message doesn't need to end with NL character.

-spec print(string(), [term()], #state{}) ->
  ok.

print(Format, Args, _State = #state{level = Level}) ->
  Indent = ["    " || _ <- lists:seq(1, Level)],
  print(standard_io, Indent, Format, Args).

%% @doc Print informational message to screen.

-spec print_info(string(), [term()], #state{}) ->
  ok.

print_info(Format, Args, _State = #state{level = Level}) ->
  Indent = case Level > 0 of
    true -> ["    # " || _ <- lists:seq(1, Level)];
    false -> "# "
  end,
  print(standard_io, Indent, Format, Args).

%% @doc Print diagnostic (warning) message to screen.

-spec print_warning(string(), [term()], #state{}) ->
  ok.

print_warning(Format, Args, _State = #state{level = Level}) ->
  Indent = case Level > 0 of
    true -> ["    # " || _ <- lists:seq(1, Level)];
    false -> "# "
  end,
  print(standard_error, Indent, Format, Args).

%% @doc Print message to specified IO device, each line indented.

-spec print(io:device(), iolist(), string(), [term()]) ->
  ok.

print(Output, Indent, Format, Args) ->
  Text = iolist_to_binary(io_lib:format(Format, Args)),
  Lines = binary:split(Text, <<"\n">>, [global, trim]),
  [io:put_chars(Output, [Indent, L, "\n"]) || L <- Lines],
  ok.

%% }}}
%%----------------------------------------------------------
%% `#counter{}' handling {{{

%% @doc Add 1 to skipped tests counter.

add_skipped(Counters = #counters{tests = T, skipped = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, skipped = N + 1}.

%% @doc Add 1 to successes or failures counter, depending on the `Result'.

add_result(success = _Result,
           Counters = #counters{tests = T, successes = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, successes = N + 1};

add_result({failure, _} = _Result,
           Counters = #counters{tests = T, failures = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, failures = N + 1};

add_result({dubious, _} = _Result,
           Counters = #counters{tests = T, failures = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, failures = N + 1};

add_result({died, _} = _Result,
           Counters = #counters{tests = T, failures = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, failures = N + 1}.

%% @doc Add 1 to TODO successes or failures counter, depending on the
%%   `Result'.

add_todo(success = _Result,
         Counters = #counters{tests = T, todo_successes = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, todo_successes = N + 1};

add_todo({failure, _} = _Result,
         Counters = #counters{tests = T, todo_failures = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, todo_failures = N + 1};

add_todo({dubious, _} = _Result,
         Counters = #counters{tests = T, todo_failures = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, todo_failures = N + 1};

add_todo({died, _} = _Result,
         Counters = #counters{tests = T, todo_failures = N}) ->
  _NewCounters = Counters#counters{tests = T + 1, todo_failures = N + 1}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
