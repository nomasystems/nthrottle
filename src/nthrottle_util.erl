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
-module(nthrottle_util).

%%% API EXPORTS
-export([from_rps/1, notify/1, safe_split/2, safe_substract/2]).

%%%-----------------------------------------------------------------------------
%%% API EXPORTS
%%%-----------------------------------------------------------------------------
from_rps(Rps) when Rps < 1 andalso Rps > 0 ->
    Inv = erlang:round(1 / Rps),
    {1, Inv * 1000};
from_rps(Rps) ->
    {Rps, 1000}.

notify([]) ->
    ok;
notify([{_N, From, Msg} | Tail]) ->
    From ! Msg,
    notify(Tail).

safe_split(infinity, List) ->
    {List, []};
safe_split(0, List) ->
    {[], List};
safe_split(N, List) when N >= length(List) ->
    {List, []};
safe_split(N, List) ->
    safe_split(N, List, []).

safe_split(0, List, A) ->
    {lists:reverse(A), List};
safe_split(N, [H | T], A) ->
    safe_split(N - 1, T, [H | A]).

safe_substract(infinity, _Substraend) ->
    infinity;
safe_substract(Minuend, Substraend) ->
    Minuend - Substraend.
