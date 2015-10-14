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
  ]),
  ?assertEqual(val, Val),
  ?assertEqual(normal, Type).

get_first_defined_test() ->
  {ok, Val, Type} = ?dac_get([
    fun() -> undefined end,
    fun() -> {ok, val1} end,
    fun() -> {ok, val2} end
  ]),
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
  ], [{default, val_default}]),
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
  ], [cached]),
  ?assertEqual(val1, Val1),
  ?assertEqual(normal, Type1),
  ?assertEqual(val1, get({dac, ?MODULE, ?current_fun()})),

  {ok, Val2, Type2} = ?dac_get([
    fun() -> {ok, val2} end
  ], [cached]),
  ?assertEqual(val1, Val2),
  ?assertEqual(cached, Type2),

  {ok, Val3, Type3} = ?dac_get([
    fun() -> {ok, val3} end
  ]),
  ?assertEqual(val3, Val3),
  ?assertEqual(normal, Type3).