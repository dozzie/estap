%%%---------------------------------------------------------------------------
%%% @doc
%%%   Input/output and test plan tracking process.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap_server).

-behaviour(gen_server).

%% public interface
-export([no_plan/0, plan/1, done/1]).
-export([running/2, join_test/1, test_skipped/3, test_todo/3]).
-export([test_passed/1, test_failed/2, dubious_result/2, test_died/2]).

%% supervision tree API
-export([start/0, start_link/0]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%%---------------------------------------------------------------------------

-type test_run_id() :: reference().

-record(state, {
}).

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc "No plan" plan.

-spec no_plan() ->
  test_run_id().

no_plan() ->
  'TODO'.

%% @doc Test plan.

-spec plan(pos_integer()) ->
  test_run_id().

plan(_TestCount) ->
  'TODO'.

%% @doc Mark the end of tests in this run.

done(_TestRunId) ->
  'TODO'.

%% @doc Mark the beginning of new test.
%%   Call this before call to test function.

running(_TestRunId, _Description) ->
  'TODO'.

%% @doc Mark the end of a test with a success.

test_passed(_TestRunId) ->
  'TODO'.

%% @doc Mark the end of a test with a failure.

test_failed(_TestRunId, _Value) ->
  'TODO'.

%% @doc Mark the end of a test with a failure, but a dubious one.

dubious_result(_TestRunId, _Value) ->
  'TODO'.

%% @doc Mark the end of a test with an exception.
%%   This means that the test function, which was called in `Pid' process,
%%   simply died.

test_died(_Pid, _Reason) ->
  % XXX: synchronous call
  'TODO'.

%% @doc Wait until all the processing of test results submitted by `Pid' is
%%   finished.

join_test(_Pid) ->
  'TODO'.

%% @doc Mark the test as skipped.

test_skipped(_TestRunId, _Description, _Reason) ->
  'TODO'.

%% @doc Mark the test as "TODO".

test_todo(_TestRunId, _Description, _Reason) ->
  'TODO'.

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the process.

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @private
%% @doc Start the process.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%---------------------------------------------------------------------------
%%% gen_server callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% initialization/termination {{{

%% @private
%% @doc Initialize event handler.

init(_Args) ->
  State = #state{},
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

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
