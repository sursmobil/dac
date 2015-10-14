%%%-------------------------------------------------------------------
%%% @author sjanota
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2015 22:13
%%%-------------------------------------------------------------------
-module(dac).
-author("CJ").

%% API
-export([get/4, env/1, trans/2, app/2]).

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
          {default, value()}.
-type options() :: [option()].
-type reader() :: fun(() -> {ok, value()} | undefined).
-type transform() :: fun((Read :: value()) -> Desired :: value()).

-export_type([value/0, value_type/0, reader/0, option/0, options/0]).

%%%-------------------------------------------------------------------
%%% Local types
%%%-------------------------------------------------------------------
-type internal_reader() ::
          reader() |
          fun(() -> {ok, value(), value_type()} | undefined).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
-spec get(module(), atom(), [reader()], options()) -> {ok, value(), value_type()} | {error, any()}.
get(Module, Property, Readers, Opts) ->
  NewReaders = parse_options(Module, Property, Readers, Opts),
  {ok, Val, Type} = do_read(Module, Property, NewReaders),
  apply_options(Module, Property, Val, Type, Opts).

-spec env(string()) -> reader().
env(Env) ->
  fun() -> case os:getenv(Env) of false -> undefined; Val -> Val end end.

-spec app(App :: atom(), Prop :: atom()) -> reader().
app(App, Prop) ->
  fun() -> case application:get_env(App, Prop) of undefined -> undefined; Val -> {ok, Val} end end.

-spec trans(reader(), transform() | [transform()]) -> reader().
trans(Reader, Transform) when not is_list(Transform) ->
  trans(Reader, [Transform]);
trans(Reader, []) ->
  Reader;
trans(Reader, [Transform | Rest]) ->
  trans(fun() -> Transform(Reader()) end, Rest).

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

-spec apply_options(module(), atom(), value(), value_type(), options()) -> [internal_reader()].
apply_options(_, _, Val, Type, []) ->
  {ok, Val, Type};
apply_options(Module, Property, Val, cached = Type, [cached | Rest]) ->
  apply_options(Module, Property, Val, Type, Rest);
apply_options(Module, Property, Val, Type, [cached | Rest]) ->
  put({dac, Module, Property}, Val),
  apply_options(Module, Property, Val, Type, Rest);
apply_options(Module, Property, Val, Type, [_ | Rest]) ->
  apply_options(Module, Property, Val, Type, Rest).