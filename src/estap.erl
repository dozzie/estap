%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions to use in test cases.
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

-spec ok(value(), description()) ->
  'TODO'.

ok(_Value, _Description) ->
  'TODO'.

-spec is(value(), value(), description()) ->
  'TODO'.

is(_Value, _Expected, _Description) ->
  'TODO'.

-spec isnt(value(), value(), description()) ->
  'TODO'.

isnt(_Value, _Expected, _Description) ->
  'TODO'.

-spec eq(value(), value(), description()) ->
  'TODO'.

eq(Value, Expected, Description) ->
  cmp(Value, '=:=', Expected, Description).

-spec ne(value(), value(), description()) ->
  'TODO'.

ne(Value, Expected, Description) ->
  cmp(Value, '=/=', Expected, Description).

-spec cmp(value(), cmp(), value(), description()) ->
  'TODO'.

cmp(_Value, _Cmp, _Expected, _Description) ->
  'TODO'.

-spec like(value(), regexp(), description()) ->
  'TODO'.

like(_Value, _Expected, _Description) ->
  'TODO'.

-spec unlike(value(), regexp(), description()) ->
  'TODO'.

unlike(_Value, _Expected, _Description) ->
  'TODO'.

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
