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
-module(nthrottle_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        throttles,
        deadlock_absence,
        not_found,
        interval
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    ct_property_test:init_per_suite(Config).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
throttles(_Conf) ->
    ThrottlerName = test_nthrottle,
    ConsumerAPI = fun Consumer(Count) ->
        receive
            stop ->
                Count
        after 0 ->
            case nthrottle:throttle(ThrottlerName) of
                ok ->
                    Consumer(Count + 1);
                rps_exceeded ->
                    Consumer(Count)
            end
        end
    end,
    RpsList = [0, 0.5, 10, 100, 1000, 10000, 100000, infinity],
    WorkTime = 300,

    lists:foreach(
        fun(Rps) ->
            nthrottle:start_throttling(ThrottlerName, Rps),
            erlang:send_after(WorkTime, self(), stop),
            {Time, Received} = timer:tc(fun() -> ConsumerAPI(0) end),
            MaxReceived =
                case Rps of
                    infinity ->
                        infinity;
                    _Otherwise ->
                        math:ceil(Time / WorkTime) * Rps
                end,
            nthrottle:stop_throttling(ThrottlerName),
            true = Received =< MaxReceived
        end,
        RpsList
    ).

deadlock_absence(Conf) ->
    ct_property_test:quickcheck(
        nthrottle_properties:prop_deadlock_absence(),
        Conf
    ).

not_found(Conf) ->
    ct_property_test:quickcheck(
        nthrottle_properties:prop_not_found(),
        Conf
    ).

interval(_Conf) ->
    ThrottlerName = test_nthrottle,
    To = self(),
    Msg = finished,
    Cycles = 3,
    Rps = 10,
    Consumers = 30,

    ConsumerAPI = fun
        Consumer(0) ->
            To ! Msg;
        Consumer(Count) ->
            receive
                continue ->
                    Consumer(Count - 1)
            after 0 ->
                nthrottle:throttle(ThrottlerName, {self(), continue}),
                Consumer(Count)
            end
    end,

    ok = nthrottle:start_throttling(ThrottlerName, Rps),
    lists:foreach(
        fun(_I) -> erlang:spawn(fun() -> ConsumerAPI(Cycles) end) end,
        lists:seq(1, Consumers)
    ),
    receive_n(Msg, Consumers),
    ok = nthrottle:stop_throttling(ThrottlerName),
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
receive_n(_Msg, 0) ->
    ok;
receive_n(Msg, N) ->
    receive
        Msg ->
            receive_n(Msg, N - 1)
    end.
