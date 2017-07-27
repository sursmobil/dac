%%%-------------------------------------------------------------------
%%% @author sjanota
%%% @doc
%%% Deployment Agnostic Configuration (DAC)
%%% DAC is used to prioritize configuration sources.
%%% @end
%%% Created : 13. Oct 2015 22:13
%%%-------------------------------------------------------------------
-module(dac).
-author("CJ").

%% API
-export([
  get/4,
  env/1,
  trans/2,
  app/2,
  l2b/0,
  l2i/0,
  l2a/0,
  onlyif/2,
  ifnot/2,
  ifdef/1,
  val/1,
  b2l/0,
  merge/4
]).

%%%-------------------------------------------------------------------
%%% Exported Types
%%%-------------------------------------------------------------------
-type value() :: any().
-type value_type() ::
          default |
          cached |
          normal.
-type option() ::
          cached |
          {default, value()} |
          verbose. %% Use this option if you want any additional information then value.
                   %% When used 'get' will return {ok, value, type}
-type merge_option() :: option() | deep.

-type options() :: [option()].
-type merge_options() :: [merge_option()].
-type reader() :: fun(() -> {ok, value()} | undefined).
-type predicate() :: fun(() -> boolean()).
-type transform() :: fun((Read :: value()) -> Desired :: value()).

-export_type([
  value/0,
  value_type/0,
  reader/0,
  option/0,
  options/0,
  predicate/0,
  merge_options/0,
  merge_option/0
]).

%%%-------------------------------------------------------------------
%%% Local types
%%%-------------------------------------------------------------------
-type internal_reader() ::
          reader() |
          fun(() -> {ok, value(), value_type()} | undefined).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
-spec get(module(), atom(), [reader()], options()) -> {ok, value(), value_type()} | value() | {error, any()}.
get(Module, Property, Readers, Opts) ->
  NewReaders = parse_options(Module, Property, Readers, Opts),
  {ok, Val, Type} = do_read(Module, Property, NewReaders),
  apply_options(Module, Property, Val, Type, Opts).

-spec merge(module(), atom(), [reader()], options()) -> {ok, value(), value_type()} | value() | {error, any()}.
merge(Module, Property, Readers, Opts) ->
  NewReaders = parse_options(Module, Property, Readers, Opts),
  {ok, Val, Type} = do_merge(Module, Property, NewReaders, Opts),
  apply_options(Module, Property, Val, Type, Opts).

%%%-------------------------------------------------------------------
%%% Utils
%%%-------------------------------------------------------------------

%% Configuration should use given reader if predicates return false
-spec ifnot(predicate() | [predicate()], any()) -> reader().
ifnot(Predicate, Value) when not is_list(Predicate) ->
  onlyif([Predicate], Value);
ifnot(Predicates, Value) ->
  onlyif([fun() -> not Pred() end || Pred <- Predicates], Value).

%% Configuration should use given reader if predicates return true
-spec onlyif(predicate() | [predicate()], any()) -> reader().
onlyif(Predicate, Reader) when not is_list(Predicate) ->
  onlyif([Predicate], Reader);
onlyif(Predicates, Reader) ->
  fun() -> case lists:all(fun(Predicate) -> Predicate() end, Predicates) of
    true -> Reader();
    false -> undefined
  end end.

%% Use environmental variable if present
-spec env(string()) -> reader().
env(Env) ->
  fun(_A) -> case os:getenv(Env) of false -> undefined; Val -> {ok, Val} end end.

%% Use application configuration if present
-spec app(App :: atom(), Prop :: atom()) -> reader().
app(App, Prop) ->
  fun() -> case application:get_env(App, Prop) of undefined -> undefined; Val -> Val end end.

%% Reader with static value. Useful with onlyif or ifnot
-spec val(Value :: value()) -> reader().
val(Value) ->
  fun() -> {ok, Value} end.

%% Apply transformation to value returned by reader (if defined). See l2* functions
-spec trans(reader(), transform() | [transform()]) -> reader().
trans(Reader, Transform) when not is_list(Transform) ->
  trans(Reader, [Transform]);
trans(Reader, []) ->
  Reader;
trans(Reader, [Transform | Rest]) ->
  trans(fun() ->
    case Reader() of
      {ok, Val} -> {ok, Transform(Val)};
      undefined -> undefined
    end
  end, Rest).

%% Use value returned by function if it is different then 'undefined'
-spec ifdef(fun(() -> value() | undefined)) -> reader().
ifdef(Fun) ->
  fun() -> case Fun() of undefined -> undefined; Other -> {ok, Other} end end.

%% Transform list to binary
-spec l2b() -> transform().
l2b() ->
  fun erlang:list_to_binary/1.

%% Transform binary to list
-spec b2l() -> transform().
b2l() ->
  fun erlang:binary_to_list/1.

%% Transform list to integer
-spec l2i() -> transform().
l2i() ->
  fun erlang:list_to_integer/1.

%% Transform list to atom
-spec l2a() -> transform().
l2a() ->
  fun erlang:list_to_atom/1.

%%%-------------------------------------------------------------------
%%% Local
%%%-------------------------------------------------------------------
-spec parse_options(module(), atom(), [internal_reader()], options()) -> [internal_reader()].
parse_options(_, _, Readers, []) ->
  Readers;
parse_options(Module, Property, Readers, [cached | Rest]) ->
  CachedReader = fun() ->
    case get({dac, Module, Property}) of
      undefined -> undefined;
      Val -> {ok, Val, cached}
    end
  end,
  parse_options(Module, Property, [CachedReader | Readers], Rest);
parse_options(Module, Property, Readers, [{default, Default} | Rest]) ->
  DefaultReader = fun() -> {ok, Default, default} end,
  parse_options(Module, Property, Readers ++ [DefaultReader], Rest);
parse_options(Module, Property, Readers, [_ | Rest]) ->
  parse_options(Module, Property, Readers, Rest).

-spec do_read(module(), atom(), [internal_reader()]) -> {ok, value(), value_type()}.
do_read(Module, Property, []) ->
  erlang:throw({config_not_present, {Module, Property}});
do_read(Module, Property, [Reader | Rest]) ->
  case Reader() of
    undefined -> do_read(Module, Property, Rest);
    {ok, Val} -> {ok, Val, normal};
    {ok, Val, Type} -> {ok, Val, Type}
  end.

-spec do_merge(module(), atom(), [internal_reader()], merge_options()) -> {ok, value(), value_type()}.
do_merge(Module, Property, Readers, Opts) ->
  do_merge(Module, Property, Readers, Opts, undefined).

-spec do_merge(module(), atom(), [internal_reader()], merge_options(), Acc :: undefined | {acc, value(), value_type()}) -> {ok, value(), value_type()}.
do_merge(Module, Property, [], Opts, undefined) ->
  erlang:throw({config_not_present, {Module, Property}});
do_merge(_Module, _Property, [], Opts, {acc, Val, Type}) ->
  {ok, Val, Type};
do_merge(Module, Property, [Reader | Rest], Opts, Acc) ->
  NewAcc = case Reader() of
    undefined -> Acc;
    {ok, Val} -> merge_in_previous(Acc, Val, normal, Opts);
    {ok, Val, Type} -> merge_in_previous(Acc, Val, Type, Opts)
  end,
  do_merge(Module, Property, Rest, Opts, NewAcc).

-spec apply_options(module(), atom(), value(), value_type(), options()) -> value() | {ok, value(), value_type()}.
apply_options(_, _, Val, _, []) ->
  Val;
apply_options(_, _, Val, Type, [verbose]) ->
  {ok, Val, Type};
apply_options(Module, Property, Val, cached = Type, [cached | Rest]) ->
  apply_options(Module, Property, Val, Type, Rest);
apply_options(Module, Property, Val, cached = Type, [verbose | Rest]) ->
  apply_options(Module, Property, Val, Type, Rest ++ [verbose]);
apply_options(Module, Property, Val, Type, [cached | Rest]) ->
  put({dac, Module, Property}, Val),
  apply_options(Module, Property, Val, Type, Rest);
apply_options(Module, Property, Val, Type, [_ | Rest]) ->
  apply_options(Module, Property, Val, Type, Rest).

merge_in_previous(undefined, Val, Type, _Opts) ->
  {acc, Val, Type};
merge_in_previous({acc, Previous, _PrevType}, New, Type, Opts) ->
  NewValue = merge_values(Previous, New, Opts),
  {acc, NewValue, Type}.

merge_values(Previous, New, Opts) when is_map(Previous) and is_map(New) ->
  case proplists:is_defined(deep, Opts) of
    true -> deep_merge_map(Previous, New, Opts);
    false -> maps:merge(New, Previous)
  end;
merge_values(Previous, New, Opts) when is_list(Previous) and is_list(New) ->
  case proplists:get_value(deep, Opts) of
    true -> Previous ++ New;
    maps -> Previous
  end;
merge_values(Previous, _New, _Opts) ->
  Previous.

deep_merge_map(Base, Overwrite, Opts) ->
  DoOverwrite = fun
    Loop([], Acc) -> Acc;
    Loop([Key | Keys], Acc) ->
      Value = maps:get(Key, Base),
      NewAcc = case maps:is_key(Key, Overwrite) of
        true ->
          OverwriteValue = maps:get(Key, Overwrite),
          NewValue = merge_values(Value, OverwriteValue, Opts),
          maps:put(Key, NewValue, Acc);
        false ->
          Acc
      end,
      Loop(Keys, NewAcc)
  end,
  AddMissing = fun
    Loop([], Acc) -> Acc;
    Loop([Key | Keys], Acc) ->
      NewAcc = case maps:is_key(Key, Acc) of
        false ->
          maps:put(Key, maps:get(Key, Overwrite), Acc);
        true ->
          Acc
      end,
      Loop(Keys, NewAcc)
  end,
  Map1 = DoOverwrite(maps:keys(Base), Base),
  AddMissing(maps:keys(Overwrite), Map1).
