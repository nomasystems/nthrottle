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
-module(nthrottle_sup).

-behaviour(supervisor).

%%% START/STOP EXPORTS
-export([start_link/0, init/1, stop/0]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    nthrottle_tab:create(),
    {ok, {{one_for_all, 1, 60}, [child(nthrottle_srv)]}}.

stop() ->
    nthrottle_tab:destroy(),
    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
child(Mod) ->
    child(Mod, []).

child(Mod, Args) ->
    {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}.
