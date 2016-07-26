%%%-------------------------------------------------------------------
%%% @author CJ
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2015 22:42
%%%-------------------------------------------------------------------
-author("CJ").

-define(current_fun(), element(2, element(2, process_info(self(), current_function)))).

-define(dac_get(Property, Readers, Opts), dac:get(?MODULE, Property, Readers, Opts)).
-define(dac_get(Readers, Opts), dac:get(?MODULE, ?current_fun(), Readers, Opts)).
-define(dac_get(Readers), dac:get(?MODULE, ?current_fun(), Readers, [])).

-define(dac_merge(Property, Readers, Opts), dac:merge(?MODULE, Property, Readers, Opts)).
-define(dac_merge(Readers, Opts), dac:merge(?MODULE, ?current_fun(), Readers, Opts)).
-define(dac_merge(Readers), dac:merge(?MODULE, ?current_fun(), Readers, [])).