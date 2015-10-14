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

basic_get_test() ->
  {ok, Val, Type} = ?dac_get([
    fun() -> {ok, val} end
  ]),
  ?assertEqual(val, Val),
  ?assertEqual(normal, Type).

error_thrown_if_value_not_found_test() ->
 ?assertThrow({config_not_present, {dac_tests, property}}, ?dac_get(property, [], [])).

get_first_defined_test() ->
  {ok, Val, Type} = ?dac_get([
    fun() -> undefined end,
    fun() -> {ok, val1} end,
    fun() -> {ok, val2} end
  ]),
  ?assertEqual(val1, Val),
  ?assertEqual(normal, Type).

use_default_test() ->
  {ok, Val, Type} = ?dac_get([
    fun() -> undefined end,
    fun() -> undefined end
  ], [{default, val_default}]),
  ?assertEqual(val_default, Val),
  ?assertEqual(default, Type).

use_cached_value_test() ->
  {ok, Val1, Type1} = ?dac_get([
    fun() -> {ok, val1} end
  ], [cached]),
  ?assertEqual(val1, Val1),
  ?assertEqual(normal, Type1),

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