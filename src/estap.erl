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
%%% @end
%%%---------------------------------------------------------------------------

-module(estap).

%% public interface
-export([ok/2, is/3, isnt/3, eq/3, ne/3, cmp/4, like/3, unlike/3, matches/3]).
-export([bail_out/1, no_plan/0, plan/1, all_ok/0]).
-export([diag/1, diag/2, note/1, note/2, explain/1]).
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

-type info() :: term(). % TODO: more detailed definition

%%% }}}
%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Check if `Value' is any of the recognized truth values.

-spec ok(value(), description()) ->
  ok.

ok(Value, Description) ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  estap_server:report_result(TestRun, estap_test:success_or_failure(Value)).

%% @doc Check if `Value' is the same as `Expected'.

-spec is(value(), value(), description()) ->
  ok.

is(Value, Expected, Description) ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  case Value of
    Expected ->
      estap_server:report_result(TestRun, {success, true});
    _ ->
      estap_server:report_result(TestRun, {failure, false})
  end.

%% @doc Check if `Value' is different than `Expected'.

-spec isnt(value(), value(), description()) ->
  ok.

isnt(Value, Expected, Description) ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  case Value of
    Expected ->
      estap_server:report_result(TestRun, {failure, false});
    _ ->
      estap_server:report_result(TestRun, {success, true})
  end.

%% @doc Check if `Value' is equal (`==') to `Expected'.

-spec eq(value(), value(), description()) ->
  ok.

eq(Value, Expected, Description) ->
  % XXX: no `get_test_run()' call
  cmp(Value, '==', Expected, Description).

%% @doc Check if `Value' is not equal (`/=') to `Expected'.

-spec ne(value(), value(), description()) ->
  ok.

ne(Value, Expected, Description) ->
  % XXX: no `get_test_run()' call
  cmp(Value, '/=', Expected, Description).

%% @doc Compare `Value' and `Expected' using comparison operator.

-spec cmp(value(), cmp(), value(), description()) ->
  ok.

cmp(Value, Cmp, Expected, Description) ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
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
  case CmpResult of
    true  -> estap_server:report_result(TestRun, {success, true});
    false -> estap_server:report_result(TestRun, {failure, false})
  end.

%% @doc Check if `Value' matches a regexp.

-spec like(value(), regexp(), description()) ->
  ok.

like(Value, Expected, Description) ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  case re:run(Value, Expected) of
    {match, _Capture} -> estap_server:report_result(TestRun, {success, true});
    nomatch           -> estap_server:report_result(TestRun, {failure, false})
  end.

%% @doc Check if `Value' not matches a regexp.

-spec unlike(value(), regexp(), description()) ->
  ok.

unlike(Value, Expected, Description) ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  case re:run(Value, Expected) of
    {match, _Capture} -> estap_server:report_result(TestRun, {failure, false});
    nomatch           -> estap_server:report_result(TestRun, {success, true})
  end.

%% @doc Check if `Value' pattern-matches.
%%   Pattern is specified as a fun that has clauses defined only for what
%%   should match, i.e., calling the fun should fail with `function_clause'
%%   error. Return value of the fun is ignored.

-spec matches(value(), match_fun(), description()) ->
  ok.

matches(Value, MatchSpec, Description) ->
  TestRun = get_test_run(),
  estap_server:running(TestRun, Description),
  try
    MatchSpec(Value),
    estap_server:report_result(TestRun, {success, true})
  catch
    error:function_clause ->
      estap_server:report_result(TestRun, {failure, false})
  end.

%%%---------------------------------------------------------------------------

%% @doc Stop testing current suite because something terrible happened.
%%
%% @TODO Implement this function.

-spec bail_out(message()) ->
  no_return().

bail_out(_Message) ->
  'TODO'.

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
  true | false.

all_ok() ->
  TestRun = get_test_run(),
  {Planned, Total, Failed, _TODO} = estap_server:get_status(TestRun),
  estap_server:done(TestRun), % this ends estap_server, so it goes last
  (Failed == 0) and ((Planned == undefined) or (Planned == Total)).

%%%---------------------------------------------------------------------------

%% @doc Get a directory containing this test script.
%%
%% @TODO Implement this function.

test_dir() ->
  'TODO'.

%% @doc Get a subdirectory of the directory containing this test script.
%%
%% @TODO Implement this function.

test_dir(_Subdir) ->
  'TODO'.

%%%---------------------------------------------------------------------------

%% @doc Print a warning.
%%
%% @TODO Implement this function.

-spec diag(message()) ->
  'TODO'.

diag(_Message) ->
  'TODO'.

%% @doc Print a warning with some context.
%%
%% @TODO Implement this function.

-spec diag(message(), [info()]) ->
  'TODO'.

diag(_Message, _Info) ->
  'TODO'.

%% @doc Print a message.
%%
%% @TODO Implement this function.

-spec note(message()) ->
  'TODO'.

note(_Message) ->
  'TODO'.

%% @doc Print a message with some context.
%%
%% @TODO Implement this function.

-spec note(message(), [info()]) ->
  'TODO'.

note(_Message, _Info) ->
  'TODO'.

%% @doc Format term so it can be printed to screen.
%%   Convenience wrapper for {@link io_lib:format/2}.
%% @spec explain(term()) ->
%%   iolist()
%%
%% @TODO Implement this function.

-spec explain(term()) ->
  'TODO'.

explain(_Term) ->
  'TODO'.

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

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
