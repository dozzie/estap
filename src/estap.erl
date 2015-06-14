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
-export([bail_out/1, plan/0, plan/1, all_ok/0]).
-export([diag/1, diag/2, note/1, note/2, explain/1]).

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
  'TODO'.

ok(_Value, _Description) ->
  'TODO'.

%% @doc Check if `Value' is the same as `Expected'.

-spec is(value(), value(), description()) ->
  'TODO'.

is(_Value, _Expected, _Description) ->
  'TODO'.

%% @doc Check if `Value' is different than `Expected'.

-spec isnt(value(), value(), description()) ->
  'TODO'.

isnt(_Value, _Expected, _Description) ->
  'TODO'.

%% @doc Check if `Value' is equal (`==') to `Expected'.

-spec eq(value(), value(), description()) ->
  'TODO'.

eq(Value, Expected, Description) ->
  cmp(Value, '==', Expected, Description).

%% @doc Check if `Value' is not equal (`/=') to `Expected'.

-spec ne(value(), value(), description()) ->
  'TODO'.

ne(Value, Expected, Description) ->
  cmp(Value, '/=', Expected, Description).

%% @doc Compare `Value' and `Expected' using comparison operator.

-spec cmp(value(), cmp(), value(), description()) ->
  'TODO'.

cmp(_Value, _Cmp, _Expected, _Description) ->
  'TODO'.

%% @doc Check if `Value' matches a regexp.

-spec like(value(), regexp(), description()) ->
  'TODO'.

like(_Value, _Expected, _Description) ->
  'TODO'.

%% @doc Check if `Value' not matches a regexp.

-spec unlike(value(), regexp(), description()) ->
  'TODO'.

unlike(_Value, _Expected, _Description) ->
  'TODO'.

%% @doc Check if `Value' pattern-matches.
%%   Pattern is specified as a fun that has clauses defined only for what
%%   should match, i.e., calling the fun should fail with `function_clause'
%%   error. Return value of the fun is ignored.

-spec matches(value(), match_fun(), description()) ->
  'TODO'.

matches(_Value, _MatchSpec, _Description) ->
  'TODO'.

%%%---------------------------------------------------------------------------

%% @doc Stop testing current suite because something terrible happened.

-spec bail_out(message()) ->
  no_return().

bail_out(_Message) ->
  'TODO'.

%% @doc Set the "no plan" plan for sub-tests.
%%   Calling this function may be safely skipped.
%%
%% @see all_ok/0

-spec plan() ->
  'TODO'.

plan() ->
  'TODO'.

%% @doc Set expected number of sub-tests.

-spec plan(pos_integer()) ->
  'TODO'.

plan(_TestCount) ->
  'TODO'.

%% @doc Check if all the current sub-tests were OK.
%%   Function intended to be called at the end of a sequence of sub-tests, to
%%   indicate that the test sequence passed or failed.

-spec all_ok() ->
  'TODO'.

all_ok() ->
  'TODO'.

%%%---------------------------------------------------------------------------

%% @doc Print a warning.

-spec diag(message()) ->
  'TODO'.

diag(_Message) ->
  'TODO'.

%% @doc Print a warning with some context.

-spec diag(message(), [info()]) ->
  'TODO'.

diag(_Message, _Info) ->
  'TODO'.

%% @doc Print a message.

-spec note(message()) ->
  'TODO'.

note(_Message) ->
  'TODO'.

%% @doc Print a message with some context.

-spec note(message(), [info()]) ->
  'TODO'.

note(_Message, _Info) ->
  'TODO'.

%% @doc Format term so it can be printed to screen.
%%   Convenience wrapper for {@link io_lib:format/2}.

-spec explain(term()) ->
  iolist().

explain(_Term) ->
  'TODO'.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
