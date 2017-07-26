%%%-------------------------------------------------------------------
%%% @author CJ
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2015 23:24
%%%-------------------------------------------------------------------
-module(dac_tests).
-author("CJ").

-include("../include/dac.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% DAC allow to order configuration readers by their importance. Reader
%%% can return {ok, Val} if it means that his configuration is present
%%% and can be applied and undefined otherwise. In case of undefined
%%% DAC asks next reader for configuration. Type is information about
%%% how this configuration was obtained. If one of readers returned it
%%% it will be 'normal'.
%%%-------------------------------------------------------------------
basic_get_test() ->
  {ok, Val, Type} = ?dac_get([
    fun() -> {ok, val} end
  ], [verbose]),
  ?assertEqual(val, Val),
  ?assertEqual(normal, Type).

get_first_defined_test() ->
  {ok, Val, Type} = ?dac_get([
    fun() -> undefned end,
    fun() -> {ok, val1} end,
    fun() -> {ok, val2} end
  ], [verbose]),
  ?assertEqual(val1, Val),
  ?assertEqual(normal, Type).


%%%-------------------------------------------------------------------
%%% If not configuration can be found exception {config_not_present, {module, property}}
%%% is thrown. Property by default is name of calling function, but it
%%% can be given explicitly.
%%%-------------------------------------------------------------------
error_thrown_if_value_not_found_test() ->
 ?assertThrow({config_not_present, {dac_tests, foo_property}}, ?dac_get(foo_property, [], [])).

%%%-------------------------------------------------------------------
%%% If default value can be applied it can be added to options. If DAC
%%% could not find any configuration it will return that value instead
%%% of throwing exception. Returned type will also be 'default' instead
%%% of 'normal'.
%%%-------------------------------------------------------------------
use_default_test() ->
  {ok, Val, Type} = ?dac_get([
    fun() -> undefined end,
    fun() -> undefined end
  ], [{default, val_default}, verbose]),
  ?assertEqual(val_default, Val),
  ?assertEqual(default, Type).

%%%-------------------------------------------------------------------
%%% DAC will not cache any values by default but always take actual ones.
%%% If caching is acceptable it can be applied using 'cached' option.
%%% DAC will then store that value in process dictionary and check for
%%% its existance every time. If read without 'cached' is be performed,
%%% cached value will be ignored.
%%%-------------------------------------------------------------------
use_cached_value_test() ->
  {ok, Val1, Type1} = ?dac_get([
    fun() -> {ok, val1} end
  ], [cached, verbose]),
  ?assertEqual(val1, Val1),
  ?assertEqual(normal, Type1),

  {ok, Val2, Type2} = ?dac_get([
    fun() -> {ok, val2} end
  ], [cached, verbose]),
  ?assertEqual(val1, Val2),
  ?assertEqual(cached, Type2),

  {ok, Val3, Type3} = ?dac_get([
    fun() -> {ok, val3} end
  ], [verbose]),
  ?assertEqual(val3, Val3),
  ?assertEqual(normal, Type3).

%%%-------------------------------------------------------------------
%%% dac module contains some commonly used readers like app to read
%%% application config or env to read environmental variable. there is
%%% also trans function which allows to applie transformation to existing
%%% reader. It can be used for example if port is read as string instead
%%% of integer (dac also has functions for that l2b for binary and l2i
%%% for integer).
%%%-------------------------------------------------------------------
utils_test() ->
  %% transform string to binary
  Val1 = ?dac_get([
    dac:trans(fun() -> {ok, "string"} end, [dac:l2b()])
  ]),
  ?assertEqual(<<"string">>, Val1),

  %% transform string to integer
  Val2 = ?dac_get([
    dac:trans(fun() -> {ok, "127"} end, [dac:l2i()])
  ]),
  ?assertEqual(127, Val2),

  %% get application env
  application:set_env(dac, my_prop, val_app),
  Val3 = ?dac_get([
    dac:app(dac, my_prop)
  ]),
  ?assertEqual(val_app, Val3).

%%%-------------------------------------------------------------------
%%% If it is not neccesary to have information abbut value returned
%%% verbose option can be ommited.
%%%-------------------------------------------------------------------
plain_value_test() ->
  Val1 = ?dac_get([
    dac:trans(fun() -> {ok, "string"} end, [dac:l2b()])
  ]),
  ?assertEqual(<<"string">>, Val1).

merge_test() ->
  Val = ?dac_merge([
    dac:val(#{val1 => 1, map => #{val2 => 2, val3 => 3}}),
    dac:val(#{map => #{val4 => 4}, val5 => 5, val1 => 12})
  ]),
  ?assertEqual(#{val1 => 1, map => #{val2 => 2, val3 => 3}, val5 => 5}, Val).

merge_deep_test() ->
  Val = ?dac_merge([
    dac:val(#{val1 => 1, map => #{val2 => 2, val3 => 3}}),
    dac:val(#{map => #{val4 => 4}, val5 => 5, val1 => 12})
  ], [deep]),
  ?assertEqual(#{val1 => 1, map => #{val4 => 4, val2 => 2, val3 => 3}, val5 => 5}, Val).
