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
-module(nthrottle).

%%% START/STOP EXPORTS
-export([start/2, prep_stop/1, stop/1]).

%%% API EXPORTS
-export([rps/1, rps/2, start_throttling/2, stop_throttling/1, throttle/1, throttle/2]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start(_Type, _StartArgs) ->
    nthrottle_sup:start_link().

prep_stop(_St) ->
    nthrottle_sup:stop().

stop(_St) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% API EXPORTS
%%%-----------------------------------------------------------------------------
-type rps() :: 'infinity' | non_neg_integer() | float().
-type subscription() :: 'undefined' | {From :: pid(), Msg :: term()}.

-spec rps(Name) -> Result when
    Name :: atom(),
    Result :: {ok, rps()} | {error, not_found}.
rps(Name) ->
    nthrottle_tab:rps(Name).

-spec rps(Name, Rps) -> Result when
    Name :: atom(),
    Rps :: rps(),
    Result :: ok | {error, not_found}.
rps(Name, Rps) ->
    nthrottle_srv:rps(Name, Rps).

-spec start_throttling(Name, Rps) -> Result when
    Name :: atom(),
    Rps :: rps(),
    Result :: ok.
start_throttling(Name, Rps) ->
    nthrottle_tab:setup(Name, Rps),
    nthrottle_srv:start_interval(Name, Rps).

-spec stop_throttling(Name) -> Result when
    Name :: atom(),
    Result :: ok | {error, not_found}.
stop_throttling(Name) ->
    case nthrottle_srv:stop_interval(Name) of
        ok ->
            nthrottle_tab:destroy(Name);
        Otherwise ->
            Otherwise
    end.

-spec throttle(Name) -> Result when
    Name :: atom(),
    Result :: ok | rps_exceeded.
throttle(Name) ->
    throttle(Name, undefined).

-spec throttle(Name, Subscription) -> Result when
    Name :: atom(),
    Subscription :: subscription(),
    Result :: ok | rps_exceeded.
throttle(Name, Subscription) ->
    case nthrottle_tab:trigger(Name) of
        -1 ->
            ok = nthrottle_tab:subscribe(Name, Subscription),
            rps_exceeded;
        _N ->
            ok
    end.
