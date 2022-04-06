%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%%
-module(nthrottle_properties).

%%% PROPERTIES
-compile([export_all, nowarn_export_all]).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
prop_deadlock_absence() ->
    ThrottlerName = test_nthrottle,
    ConsumerAPI = fun Consumer() ->
        receive
            throttle ->
                nthrottle:throttle(ThrottlerName, {self(), continue});
            rps ->
                nthrottle:rps(ThrottlerName);
            {rps, Rps} ->
                nthrottle:rps(ThrottlerName, Rps);
            continue ->
                ok
        after
            3000 ->
                erlang:error("Possible deadlock!")
        end,
        Consumer()
    end,
    ConsumersNumType = triq_dom:int(1, 1000),
    RpsType = triq_dom:oneof([infinity, triq_dom:int(0, 100000)]),
    CommandsType = triq_dom:non_empty(
        triq_dom:list(
            triq_dom:oneof([
                throttle,
                rps,
                {rps, RpsType}
            ])
        )
    ),

    ?FORALL(
        {ConsumersNum, InitialRps, Commands},
        {ConsumersNumType, RpsType, CommandsType},
        begin
            nthrottle:start_throttling(ThrottlerName, InitialRps),
            Consumers = lists:map(fun(_I) -> erlang:spawn(ConsumerAPI) end, lists:seq(1, ConsumersNum)),
            lists:foreach(
                fun(Command) ->
                    Consumer = lists:nth(rand:uniform(ConsumersNum), Consumers),
                    Consumer ! Command
                end,
                Commands
            ),
            nthrottle:stop_throttling(ThrottlerName),
            true
        end
    ).

prop_not_found() ->
    ?FORALL(
        Name,
        triq_dom:atom(),
        begin
            {error, not_found} = nthrottle:rps(Name),
            {error, not_found} = nthrottle:rps(Name, 10),
            {error, not_found} = nthrottle:stop_throttling(Name),
            true
        end
    ).
