%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions to use in test cases.
%%%
%%%   Test passes if it returns or throws (`throw()'): `ok', `{ok, Value}', or
%%%   `true'.
%%%
%%%   Test fails if it returns or throws `error', `{error, Reason}', `false',
%%%   or calls `exit(...)' or `erlang:error(...)' (or simply dies).
%%%
%%%   Any other returned value is also considered a failure, but a dubious
%%%   one. Stick to saying explicitly that the test failed.
%%%
%%%   All test functions return the value they were passed (an exception to
%%%   this rule are {@link pass/1} and {@link fail/1} functions, for obvious
%%%   reason). This allows test to be added around preparation function call
%%%   for more complex test cases.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap).

%% public interface
-export([ok/2, not_ok/2, error/2]).
-export([pass/1, fail/1]).
-export([is/3, isnt/3, eq/3, ne/3, cmp/4]).
-export([like/3, unlike/3, matches/3]).
-export([bail_out/1, no_plan/0, plan/1, all_ok/0]).
-export([diag/1, diag/2, info/1, info/2, explain/1]).
-export([test_dir/0, test_dir/1]).

-export_type([value/0, cmp/0, regexp/0, match_fun/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type value() :: term().

-type cmp() :: '<' | '>' | '=<' | '>=' | '/=' | '=/=' | '==' | '=:='.

-type regexp() :: iolist().

-type match_fun() :: fun((value()) -> any()).

-type message() :: iolist().

-type description() :: iolist().

-type info() :: atom() | iolist() |
                {FieldName :: atom() | iolist(), Value :: atom() | iolist()}.

%%% }}}
%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Check if `Value' is any of the recognized truth values.
%%
%% @see not_ok/2
%% @see error/2

-spec ok(value(), description()) ->
  Value :: value().

ok(Value, Description) ->
  report(estap_test:success_or_failure(Value), Description),
  Value.

%% @doc Check if `Value' is none of the recognized truth values.
%%
%%   Dubious `Value' is also a success here.
%%
%% @see ok/2
%% @see error/2

-spec not_ok(value(), description()) ->
  Value :: value().

not_ok(Value, Description) ->
  case estap_test:success_or_failure(Value) of
    {success, _} -> report({failure, Value}, Description);
    {failure, _} -> report({success, Value}, Description);
    {dubious, _} -> report({success, Value}, Description)
  end,
  Value.

%% @doc Check if `Value' is any of the recognized falsity values.
%%
%% @see ok/2
%% @see not_ok/2

-spec error(value(), description()) ->
  Value :: value().

error(Value, Description) ->
  case estap_test:success_or_failure(Value) of
    {success, _} -> report({failure, Value}, Description);
    {failure, _} -> report({success, Value}, Description);
    {dubious, _} -> report({dubious, Value}, Description)
  end,
  Value.

%% @doc Mark the test as a success unconditionally.

-spec pass(description()) ->
  true.

pass(Description) ->
  report({success, explicit}, Description),
  true.

%% @doc Mark the test as a failure unconditionally.

-spec fail(description()) ->
  false.

fail(Description) ->
  report({failure, explicit}, Description),
  false.

%% @doc Check if `Value' is the same as `Expected'.

-spec is(value(), value(), description()) ->
  Value :: value().

is(Value, Expected, Description) ->
  case Value of
    Expected -> report({success, Value}, Description);
    _        -> report({failure, Value}, Description)
  end,
  Value.

%% @doc Check if `Value' is different than `Expected'.

-spec isnt(value(), value(), description()) ->
  Value :: value().

isnt(Value, Expected, Description) ->
  case Value of
    Expected -> report({failure, Value}, Description);
    _        -> report({success, Value}, Description)
  end,
  Value.

%% @doc Check if `Value' is equal (`==') to `Expected'.

-spec eq(value(), value(), description()) ->
  Value :: value().

eq(Value, Expected, Description) ->
  cmp(Value, '==', Expected, Description).

%% @doc Check if `Value' is not equal (`/=') to `Expected'.

-spec ne(value(), value(), description()) ->
  Value :: value().

ne(Value, Expected, Description) ->
  cmp(Value, '/=', Expected, Description).

%% @doc Compare `Value' and `Expected' using comparison operator.

-spec cmp(value(), cmp(), value(), description()) ->
  Value :: value().

cmp(Value, Cmp, Expected, Description) ->
  CmpResult = case Cmp of
    '<'   -> Value <   Expected;
    '>'   -> Value >   Expected;
    '=<'  -> Value =<  Expected;
    '>='  -> Value >=  Expected;
    '/='  -> Value /=  Expected;
    '=/=' -> Value =/= Expected;
    '=='  -> Value ==  Expected;
    '=:=' -> Value =:= Expected
  end,
  % FIXME: better reporting for failures
  case CmpResult of
    true  -> report({success, Value}, Description);
    false -> report({failure, Value}, Description)
  end,
  Value.

%% @doc Check if `Value' matches a regexp.

-spec like(value(), regexp(), description()) ->
  Value :: value().

like(Value, Expected, Description) ->
  % XXX: regular expression may be invalid, so prepare estap_server before
  % running the regexp
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  case re:run(Value, Expected) of
    {match, _Capture} -> estap_server:report_result(TestRun, {success, Value});
    nomatch           -> estap_server:report_result(TestRun, {failure, Value})
  end,
  Value.

%% @doc Check if `Value' not matches a regexp.

-spec unlike(value(), regexp(), description()) ->
  Value :: value().

unlike(Value, Expected, Description) ->
  % XXX: regular expression may be invalid, so prepare estap_server before
  % running the regexp
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  case re:run(Value, Expected) of
    {match, _Capture} -> estap_server:report_result(TestRun, {failure, Value});
    nomatch           -> estap_server:report_result(TestRun, {success, Value})
  end,
  Value.

%% @doc Check if `Value' pattern-matches.
%%   Pattern is specified as a fun that has clauses defined only for what
%%   should match, i.e., calling the fun should fail with `function_clause'
%%   error. Return value of the fun is ignored.

-spec matches(value(), match_fun(), description()) ->
  Value :: value().

matches(Value, MatchSpec, Description) ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  try
    MatchSpec(Value),
    estap_server:report_result(TestRun, {success, Value})
  catch
    error:function_clause ->
      estap_server:report_result(TestRun, {failure, Value})
  end,
  Value.

%%%---------------------------------------------------------------------------

%% @doc Stop testing whatsoever because something terrible happened.
%%
%%   Note that bailing out is a very severe operation. It aborts all test
%%   cases, including the ones in other scripts that were not executed yet.
%%   It should be only used when an error that occurred renders whole test
%%   suite unusable before it's fixed.

-spec bail_out(message()) ->
  no_return().

bail_out(Message) ->
  TestRun = get_test_run_or_parent(),
  estap_server:bail_out(TestRun, Message),
  exit('BAIL_OUT').

%% @doc Set the "no plan" plan for sub-tests.
%%   Calling this function may be safely skipped.
%%
%% @see all_ok/0

-spec no_plan() ->
  ok.

no_plan() ->
  _TestRun = get_test_run(),
  ok.

%% @doc Set expected number of sub-tests.

-spec plan(pos_integer()) ->
  ok.

plan(TestCount) when is_integer(TestCount) ->
  TestRun = estap_server:subplan(TestCount, 1),
  set_test_run(TestRun),
  ok.

%% @doc Check if all the current sub-tests were OK.
%%   Function intended to be called at the end of a sequence of sub-tests, to
%%   indicate that the test sequence passed or failed.

-spec all_ok() ->
  ok | {error, term()}.

all_ok() ->
  TestRun = get_test_run(),
  {Planned, Total, Failed, _TODO} = estap_server:get_status(TestRun),
  estap_server:done(TestRun), % this ends estap_server, so it goes last
  case Failed of
    0 when Planned == undefined ->
      ok;
    0 when Planned == Total ->
      ok;
    0 when Planned /= Total ->
      {error, {bad_plan, [{plan, Planned}, {run, Total}]}};
    _ ->
      {error, {failures, [{failed, Failed}, {plan, Planned}, {run, Total}]}}
  end.

%%%---------------------------------------------------------------------------

%% @doc Get a directory containing this test script.
%%
%%   <b>NOTE</b>: This function doesn't work in processes spawned from test
%%   function. You need to get the directory in parent process and pass it as
%%   an argument.

test_dir() ->
  case get(test_dir) of
    undefined -> erlang:error({undefined, test_dir});
    Directory -> Directory
  end.

%% @doc Get a subdirectory of the directory containing this test script.
%%
%%   <b>NOTE</b>: This function doesn't work in processes spawned from test
%%   function. You need to get the directory in parent process and pass it as
%%   an argument.

test_dir(Subdir) ->
  filename:join(test_dir(), Subdir).

%%%---------------------------------------------------------------------------

%% @doc Print a diagnostic message.
%%   Typically, diagnostic message is a warning, but may be notice important
%%   enough to print it along with test progress by TAP consumer.
%%
%%   Before first call to {@link plan/1}, {@link no_plan/0} or test functions
%%   ({@link ok/2}, {@link is/3} etc.) message is printed at the level of
%%   parent test. After any of those, it's printed at sub-test level.
%%
%%   Normally diagnostic output goes to <i>STDERR</i>, but under TODO tests it
%%   goes to <i>STDOUT</i>.
%%
%% @TODO Make the diagnostic output go to <i>STDOUT</i> under TODO

-spec diag(message()) ->
  ok.

diag(Message) ->
  TestRun = get_test_run_or_parent(),
  estap_server:warning(TestRun, Message).

%% @doc Print a warning with some context.
%%   Typically, diagnostic message is a warning, but may be notice important
%%   enough to print it along with test progress by TAP consumer.
%%
%%   Before first call to {@link plan/1}, {@link no_plan/0} or test functions
%%   ({@link ok/2}, {@link is/3} etc.) message is printed at the level of
%%   parent test. After any of those, it's printed at sub-test level.
%%
%%   Normally diagnostic output goes to <i>STDERR</i>, but under TODO tests it
%%   goes to <i>STDOUT</i>.
%%
%% @TODO Make the diagnostic output go to <i>STDOUT</i> under TODO

-spec diag(message(), [info()]) ->
  ok.

diag(Message, Info) ->
  TestRun = get_test_run_or_parent(),
  InfoLines = [["  ", format_info(I), "\n"] || I <- Info],
  estap_server:warning(TestRun, [Message, "\n", InfoLines]).

%% @doc Print a message.
%%
%%   Before first call to {@link plan/1}, {@link no_plan/0} or test functions
%%   ({@link ok/2}, {@link is/3} etc.) message is printed at the level of
%%   parent test. After any of those, it's printed at sub-test level.

-spec info(message()) ->
  ok.

info(Message) ->
  TestRun = get_test_run_or_parent(),
  estap_server:info(TestRun, Message).

%% @doc Print a message with some context.
%%
%%   Before first call to {@link plan/1}, {@link no_plan/0} or test functions
%%   ({@link ok/2}, {@link is/3} etc.) message is printed at the level of
%%   parent test. After any of those, it's printed at sub-test level.

-spec info(message(), [info()]) ->
  ok.

info(Message, Info) ->
  TestRun = get_test_run_or_parent(),
  InfoLines = [["  ", format_info(I), "\n"] || I <- Info],
  estap_server:info(TestRun, [Message, "\n", InfoLines]).

%% @doc Format a single info entry for printing it on screen.

-spec format_info(info()) ->
  binary().

format_info(Info) when is_list(Info); is_binary(Info) ->
  iolist_to_binary(Info);
format_info(Info) when is_atom(Info) ->
  atom_to_binary(Info, unicode);
format_info({K, V} = _Info) ->
  <<(format_info(K))/binary, ": ", (format_info(V))/binary>>.

%% @doc Format term so it can be printed to screen.
%%   Convenience wrapper for {@link io_lib:format/2}.
%%
%% @see info/2
%% @see diag/2

-spec explain(term()) ->
  iolist().

explain(Term) ->
  % no term should weigh 1MB
  io_lib:print(Term, 1, 1024 * 1024, -1).

%%%---------------------------------------------------------------------------

%% @doc Set previously started {@link estap_server}.

set_test_run(TestRun) ->
  put(estap_server, TestRun).

%% @doc Get associated {@link estap_server}, starting it if necessary.

get_test_run() ->
  case get(estap_server) of
    undefined ->
      TestRun = estap_server:subplan(no_plan, 1),
      put(estap_server, TestRun),
      TestRun;
    TestRun when is_pid(TestRun) ->
      TestRun
  end.

%% @doc Get associated {@link estap_server} or parent one if none is started
%%   yet. Necessary for top-level {@link info/1} and {@link diag/1} to work.

get_test_run_or_parent() ->
  case get(estap_server) of
    undefined ->
      % XXX: this must be set in `estap_test:run()'
      get(estap_server_parent);
    TestRun when is_pid(TestRun) ->
      TestRun
  end.

%% @doc Send a test report to {@link estap_server}.

-spec report({success | failure | dubious, Value :: value()}, description()) ->
  ok.

report({T,_V} = Report, Description)
when T == success; T == failure; T == dubious ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  estap_server:report_result(TestRun, Report),
  ok.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
