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
-module(nthrottle_tab).

%%% CREATE/DESTROY EXPORTS
-export([
    create/0,
    destroy/0,
    destroy/1,
    setup/2
]).

%%% EXTERNAL EXPORTS
-export([
    rps/1,
    rps/2,
    subscribe/2,
    subscribers/2,
    trigger/1,
    triggers/2
]).

%%% MACROS
-define(TAB, nthrottle_ets).
-define(STAB, nthrottle_sets).
-define(RPS, nthrottle_rps).

%%%-----------------------------------------------------------------------------
%%% CREATE/DESTROY EXPORTS
%%%-----------------------------------------------------------------------------
create() ->
    ?TAB = ets:new(?TAB, [
        set,
        named_table,
        public,
        {decentralized_counters, true}
    ]),
    ?STAB = ets:new(?STAB, [
        bag,
        named_table,
        public
    ]),
    ok.

destroy() ->
    true = ets:delete(?TAB),
    true = ets:delete(?STAB),
    ok.

destroy(Name) ->
    true = persistent_term:erase({?RPS, Name}),
    true = ets:delete(?TAB, Name),
    true = ets:delete(?STAB, Name),
    ok.

setup(Name, Rps) ->
    {Max, _Time} = nthrottle_util:from_rps(Rps),
    rps(Name, Rps),
    triggers(Name, Max).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
rps(Name) ->
    try
        Rps = persistent_term:get({?RPS, Name}),
        {ok, Rps}
    catch
        _Class:_Error ->
            {error, not_found}
    end.

rps(Name, Rps) ->
    ok = persistent_term:put({?RPS, Name}, Rps).

subscribe(_Name, undefined) ->
    ok;
subscribe(Name, {From, Msg}) ->
    upsert(?STAB, {Name, From, Msg}).

subscribers(Name, N) ->
    AllSubscriptions = ets:lookup(?STAB, Name),
    {Subscribers, _Tail} = nthrottle_util:safe_split(N, AllSubscriptions),
    lists:foreach(fun(Subscriber) -> ets:delete_object(?STAB, Subscriber) end, Subscribers),
    Subscribers.

trigger(Name) ->
    case persistent_term:get({?RPS, Name}) of
        infinity ->
            1;
        _Rps ->
            try
                ets:update_counter(?TAB, Name, {2, -1, -1, -1})
            catch
                error:badarg ->
                    1
            end
    end.

triggers(Name, Triggers) ->
    upsert(?TAB, {Name, Triggers}).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
upsert(Tab, ObjectOrObjects) ->
    true = ets:insert(Tab, ObjectOrObjects),
    ok.
