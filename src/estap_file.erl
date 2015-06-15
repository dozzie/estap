%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions to work with files.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap_file).

%% public interface
-export([tempdir/0, tempdir/1]).
-export([read_file/2, read_file/3]).

-export([load_code/1]).

%%%---------------------------------------------------------------------------
%%% types and definitions {{{

-define(STEM_PREFIX, "estap").
-define(STEM_LEN, 8).
-define(DEFAULT_TMP, "/tmp").

-record(test, {
  %% TODO: `-prep(fun/0)', `-cleanup(fun/1)'
  name :: atom(),
  desc :: string(),
  todo = false :: {true, Reason :: string()} | false,
  skip = false :: {true, Reason :: string()} | false
}).

%%% }}}
%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% temporary directories {{{

%% @doc Create a temporary directory of unique name.
%%   The directory is created under `$TMP', or `/tmp' if the variable is not
%%   set.

-spec tempdir() ->
  {ok, file:filename()} | {error, term()}.

tempdir() ->
  case os:getenv("TMP") of
    Dir when is_list(Dir) ->
      tempdir(Dir);
    false ->
      tempdir(?DEFAULT_TMP)
  end.

%% @doc Create a temporary directory of unique name under specified directory.

-spec tempdir(file:name()) ->
  {ok, file:filename()} | {error, term()}.

tempdir(TempDir) ->
  StemSeed = crypto:sha(term_to_binary(make_ref())),
  <<Stem:?STEM_LEN/binary, _/binary>> = base64:encode(StemSeed),
  DirName = filename:join(TempDir, ?STEM_PREFIX ++ "." ++ binary_to_list(Stem)),
  case file:make_dir(DirName) of
    ok ->
      {ok, DirName};
    {error, eexist} ->
      % try again, maybe next one will succeed
      tempdir(TempDir);
    {error, Reason} ->
      {error, Reason}
  end.

%% }}}
%%----------------------------------------------------------
%% loading test files {{{

%% @doc Load estap file as ABF forms.

-spec read_file(file:name(), [file:name()]) ->
  {ok, {module(), [erl_parse:abstract_form()]}} | {error, term()}.

read_file(File, IncludePath) ->
  read_file(File, IncludePath, ?DEFAULT_TMP).

%% @doc Load estap file as ABF forms.

-spec read_file(file:name(), [file:name()], file:name()) ->
  {ok, {module(), [erl_parse:abstract_form()]}} | {error, term()}.

read_file(File, IncludePath, TempDir) ->
  case tempdir(TempDir) of
    {ok, DirName} ->
      case copy_source(File, DirName) of
        {ok, ModuleFile} ->
          Result = parse_file(ModuleFile, IncludePath),
          ok = file:delete(ModuleFile),
          ok = file:del_dir(DirName),
          Result;
        {error, Reason} ->
          ok = file:del_dir(TempDir),
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Copy estap test source to target directory, stripping shee-bang lines.

-spec copy_source(file:name(), file:name()) ->
  {ok, file:name()} | {error, term()}.

copy_source(Source, TargetDir) ->
  Target = filename:join(TargetDir, filename:basename(Source)),
  case file:read_file(Source) of
    {ok, <<"#!", _/binary>> = Content} ->
      % the file starts with shee-bang; skip this line
      case binary:split(Content, <<"\n">>) of
        [_SheeBang] ->
          {error, no_source};
        [_SheeBang, SourceCode] ->
          case file:write_file(Target, SourceCode) of
            ok -> {ok, Target};
            {error, Reason} -> {error, Reason}
          end
      end;
    {ok, Content} ->
      case file:write_file(Target, Content) of
        ok -> {ok, Target};
        {error, Reason} -> {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Parse specified file to ABF forms.

-spec parse_file(file:name(), [file:name()]) ->
  {ok, {module(), [erl_parse:abstract_form()]}} | {error, term()}.

parse_file(File, IncludePath) ->
  Macros = [],
  case epp:parse_file(File, IncludePath, Macros) of
    {ok, Forms} ->
      case [M || {attribute, _, module, M} <- Forms] of
        % in case of two `-module()' entries let the `compile:forms()' raise
        % an error
        [ModuleName | _] ->
          {ok, {ModuleName, Forms}};
        [] ->
          ModuleName = list_to_atom(filename:rootname(filename:basename(File))),
          {ok, {ModuleName, [{attribute, 0, module, ModuleName} | Forms]}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% }}}
%%----------------------------------------------------------
%% ABF handling functions {{{

%% @doc Load ABFs as a callable module.
%%   Function returns list of tests to run, in order of their appearance.

-spec load_code([erl_parse:abstract_form()]) ->
    {ok, {estap_test:test_plan(), [estap_test:test()]}}
  | {error, sticky_directory | not_purged}.

load_code(Forms) ->
  Exports = sets:from_list(exports(Forms)),
  Tests = tests(Forms),
  MissingTestExports = [
    {Fun, 0} ||
    #test{name = Fun} <- Tests,
    not sets:is_element({Fun, 0}, Exports)
  ],
  % drop all occurrences of `-test()', `-todo()', and `-skip()'
  ToCompile = lists:filter(
    fun
      ({attribute, _, A, _}) when A == test; A == todo; A == skip -> false;
      (_) -> true
    end,
    insert_exports(MissingTestExports, Forms)
  ),
  case compile:forms(ToCompile) of
    {ok, Module, Binary} ->
      case code:load_binary(Module, "", Binary) of
        {module, Module} ->
          % TODO: indicate whether anything uses the old code
          code:soft_purge(Module),
          TestsToReturn = lists:map(
            fun
              (#test{name = Name, desc = Desc, todo = {true, Why}}) ->
                {{Module, Name}, Desc, {todo, Why}};
              (#test{name = Name, desc = Desc, skip = {true, Why}}) ->
                {{Module, Name}, Desc, {skip, Why}};
              (#test{name = Name, desc = Desc, todo = false, skip = false}) ->
                {{Module, Name}, Desc, run}
            end,
            Tests
          ),
          case proplists:get_value(plan, Module:module_info(attributes)) of
            [TestCount] when is_integer(TestCount), TestCount > 0 ->
              Plan = {plan, TestCount};
            _ ->
              Plan = no_plan
          end,
          {ok, {Plan, TestsToReturn}};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Insert specified exports in list of ABFs for the module.

insert_exports(Exports, [{attribute,_,module,_} = Attr | Rest] = _Forms) ->
  [Attr, {attribute, 0, export, Exports} | Rest];
insert_exports(Exports, [Attr | Rest] = _Forms) ->
  [Attr | insert_exports(Exports, Rest)].

%% @doc Extract from list of ABFs functions that are tests to be run.

-spec tests([erl_parse:abstract_form()]) ->
  [#test{}].

tests(Forms) ->
  tests(Forms, #test{}).

%% @doc Extract from list of ABFs functions that are tests to be run.
%%   Worker function for {@link tests/1}.

-spec tests([erl_parse:abstract_form()], #test{}) ->
  [#test{}].

tests([] = _Forms, _Test) ->
  [];

tests([{attribute, _Line, test, Desc} | Rest] = _Forms, Test) ->
  tests(Rest, Test#test{desc = Desc});

tests([{attribute, _Line, todo, Reason} | Rest] = _Forms, Test) ->
  tests(Rest, Test#test{todo = {true, Reason}});

tests([{attribute, _Line, skip, Reason} | Rest] = _Forms, Test) ->
  tests(Rest, Test#test{skip = {true, Reason}});

tests([{function, _Line, FName, 0, _Body} | Rest] = _Forms, Test) ->
  case Test of
    #test{desc = undefined} ->
      % TODO: check if `FName' ends with `"_test"'
      tests(Rest, Test);
    #test{desc = Desc} when is_list(Desc) ->
      [Test#test{name = FName} | tests(Rest, #test{})]
  end;

tests([{function, _Line, _FName, _Arity, _Body} | Rest] = _Forms, _Test) ->
  % reset attributes
  tests(Rest, #test{});

tests([_Any | Rest] = _Forms, Test) ->
  tests(Rest, Test).

%% @doc Extract exports from the module.

-spec exports([erl_parse:abstract_form()]) ->
  [{atom(), byte()}].

exports(Forms) ->
  Exports = [Fs || {attribute, _Line, export, Fs} <- Forms],
  lists:flatten(Exports).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
