-module(rebar3_elixirc).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_elixirc_prv:init(State),
    {ok, State1}.
