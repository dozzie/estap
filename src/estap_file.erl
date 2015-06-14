%%%---------------------------------------------------------------------------
%%% @doc
%%%   Functions to work with files.
%%% @end
%%%---------------------------------------------------------------------------

-module(estap_file).

%% public interface
-export([tempdir/0, tempdir/1]).
-export([load_file/3]).

%%%---------------------------------------------------------------------------
%%% types and definitions {{{

-define(STEM_PREFIX, "estap").
-define(STEM_LEN, 8).

%%% }}}
%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

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
      tempdir("/tmp")
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

%% @doc Load estap file as ABF forms.

-spec load_file(file:name(), [file:name()], file:name()) ->
  {ok, {module(), [erl_parse:abstract_form()]}} | {error, term()}.

load_file(File, IncludePath, TempDir) ->
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

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
