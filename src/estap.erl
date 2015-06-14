%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions to use in test cases.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap).

%% public interface
-export([ok/2, is/3, isnt/3, eq/3, ne/3, cmp/4, like/3, unlike/3, matches/3]).

-export_type([value/0, cmp/0, regexp/0, match_fun/0]).

%%%---------------------------------------------------------------------------
%%% types {{{

-type value() :: term().

-type cmp() :: '<' | '>' | '=<' | '>=' | '/=' | '=/=' | '==' | '=:='.

-type regexp() :: iolist().

-type match_fun() :: fun((value()) -> any()).

%%% }}}
%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

-spec ok(value(), iolist()) ->
  'TODO'.

ok(_Value, _Description) ->
  'TODO'.

-spec is(value(), value(), iolist()) ->
  'TODO'.

is(_Value, _Expected, _Description) ->
  'TODO'.

-spec isnt(value(), value(), iolist()) ->
  'TODO'.

isnt(_Value, _Expected, _Description) ->
  'TODO'.

-spec eq(value(), value(), iolist()) ->
  'TODO'.

eq(Value, Expected, Description) ->
  cmp(Value, '=:=', Expected, Description).

-spec ne(value(), value(), iolist()) ->
  'TODO'.

ne(Value, Expected, Description) ->
  cmp(Value, '=/=', Expected, Description).

-spec cmp(value(), cmp(), value(), iolist()) ->
  'TODO'.

cmp(_Value, _Cmp, _Expected, _Description) ->
  'TODO'.

-spec like(value(), regexp(), iolist()) ->
  'TODO'.

like(_Value, _Expected, _Description) ->
  'TODO'.

-spec unlike(value(), regexp(), iolist()) ->
  'TODO'.

unlike(_Value, _Expected, _Description) ->
  'TODO'.

-spec matches(value(), match_fun(), iolist()) ->
  'TODO'.

matches(_Value, _MatchSpec, _Description) ->
  'TODO'.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
