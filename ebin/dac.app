%%%-------------------------------------------------------------------
%%% @author CJ
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2015 23:31
%%%-------------------------------------------------------------------
{application, dac, [
  {description, "Deployment agnostic configuration"},
  {vsn, "1"},
  {registered, []},
  {applications, [
    kernel,
    stdlib
  ]},
  {modules, [dac]}
  {env, []}
]}.