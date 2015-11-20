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
-export([get/4, env/1, trans/2, app/2, l2b/0, l2i/0, l2a/0, must/2, mustnt/2, ifdef/1]).

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
          verbose. %% Use this option if you want any aditional information then value.
                   %% Whnen used 'get' will return {ok, value, type}
-type options() :: [option()].
-type reader() :: fun(() -> {ok, value()} | undefined).
-type predicate() :: fun(() -> boolean()).
-type transform() :: fun((Read :: value()) -> Desired :: value()).

-export_type([value/0, value_type/0, reader/0, option/0, options/0, predicate/0]).

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

-spec mustnt(predicate() | [predicate()], any()) -> reader().
mustnt(Predicate, Value) when not is_list(Predicate) ->
  must([Predicate], Value);
mustnt(Predicates, Value) ->
  must([fun() -> not Pred() end || Pred <- Predicates], Value).

-spec must(predicate() | [predicate()], any()) -> reader().
must(Predicate, Value) when not is_list(Predicate) ->
  must([Predicate], Value);
must(Predicates, Value) ->
  fun() -> case lists:all(fun(Predicate) -> Predicate() end, Predicates) of
    true -> {ok, Value};
    false -> undefined
  end end.

-spec env(string()) -> reader().
env(Env) ->
  fun() -> case os:getenv(Env) of false -> undefined; Val -> {ok, Val} end end.

-spec app(App :: atom(), Prop :: atom()) -> reader().
app(App, Prop) ->
  fun() -> case application:get_env(App, Prop) of undefined -> undefined; Val -> Val end end.

-spec trans(reader(), transform() | [transform()]) -> reader().
trans(Reader, Transform) when not is_list(Transform) ->
  trans(Reader, [Transform]);
trans(Reader, []) ->
  Reader;
trans(Reader, [Transform | Rest]) ->
  trans(fun() ->
    case Reader() of
      {ok, Val} -> Transform(Val);
      undefined -> undefined
    end
  end, Rest).

-spec ifdef(fun(() -> value() | undefined)) -> reader().
ifdef(Fun) ->
  fun() -> case Fun() of undefined -> undefined; Other -> {ok, Other} end.

-spec l2b() -> transform().
l2b() ->
  fun(List) -> {ok, erlang:list_to_binary(List)} end.

-spec l2i() -> transform().
l2i() ->
  fun(List) -> {ok, erlang:list_to_integer(List)} end.

-spec l2a() -> transform().
l2a() ->
  fun(List) -> {ok, erlang:list_to_atom(List)} end.

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