%%%---------------------------------------------------------------------------
%%% @doc
%%%   Input/output and test plan tracking process.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap_server).

-behaviour(gen_server).

%% public interface
-export([no_plan/0, plan/1, done/1]).
-export([running/2]).
-export([test_skipped/2, test_todo/2]).
-export([test_passed/1, test_failed/2, dubious_result/2, test_died/2]).

%% supervision tree API
-export([start/1, start_link/1]).

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
  test :: {TestNo :: pos_integer(), Description :: string()}
}).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc "No plan" plan.

-spec no_plan() ->
  test_run_id().

no_plan() ->
  {ok, Pid} = start_link(no_plan),
  Pid.

%% @doc Test plan.

-spec plan(pos_integer()) ->
  test_run_id().

plan(TestCount) when is_integer(TestCount) ->
  {ok, Pid} = start_link(TestCount),
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

%% @doc Mark the end of a test with a success.

-spec test_passed(test_run_id()) ->
  ok.

test_passed(TestRunId) ->
  gen_server:call(TestRunId, {result, success}).

%% @doc Mark the end of a test with a failure.

-spec test_failed(test_run_id(), term()) ->
  ok.

test_failed(TestRunId, ReturnValue) ->
  gen_server:call(TestRunId, {result, {failure, ReturnValue}}).

%% @doc Mark the end of a test with a failure, but a dubious one.

-spec dubious_result(test_run_id(), term()) ->
  ok.

dubious_result(TestRunId, ReturnValue) ->
  gen_server:call(TestRunId, {result, {dubious, ReturnValue}}).

%% @doc Mark the end of a test with an exception.
%%   This means that the test function, which was called in `Pid' process,
%%   simply died.

-spec test_died(test_run_id(), term()) ->
  ok.

test_died(TestRunId, Reason) ->
  gen_server:call(TestRunId, {result, {died, Reason}}).

%% @doc Mark the test as skipped.

-spec test_skipped(test_run_id(), string()) ->
  ok.

test_skipped(TestRunId, Reason) ->
  gen_server:call(TestRunId, {skipped, Reason}).

%% @doc Mark the test as "TODO".

-spec test_todo(test_run_id(), string()) ->
  ok.

test_todo(TestRunId, Reason) ->
  gen_server:call(TestRunId, {todo, Reason}).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the process.

-spec start(plan()) ->
  {ok, pid()} | {error, term()}.

start(Plan) ->
  gen_server:start(?MODULE, [Plan], []).

%% @private
%% @doc Start the process.

-spec start_link(plan()) ->
  {ok, pid()} | {error, term()}.

start_link(Plan) ->
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
  %io:fwrite("# running test ~B: ~s~n", [NextTestNo, Desc]),
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

handle_call({skipped, Reason} = _Request, _From,
            State = #state{test = {TestNo, TestDesc}}) ->
  % TODO: OK or NOK?
  io:fwrite("ok ~B - ~s # SKIP: ~s~n", [TestNo, TestDesc, Reason]),
  {reply, ok, State};

handle_call({todo, Reason} = _Request, _From,
            State = #state{test = {TestNo, TestDesc}}) ->
  % TODO: OK or NOK?
  io:fwrite("ok ~B - ~s # TODO: ~s~n", [TestNo, TestDesc, Reason]),
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
