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
-module(nthrottle_srv).

-behaviour(gen_server).

%%% INIT/TERMINATE EXPORTS
-export([start_link/0, init/1, terminate/2]).

%%% API EXPORTS
-export([start_interval/2, stop_interval/1, rps/2]).

%%% INTERVAL EXPORTS
-export([renew_interval/1]).

%%% GENSERVER EXPORTS
-export([handle_call/3, handle_cast/2]).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, maps:new()}.

terminate(_Reason, State) ->
    lists:foreach(fun(TRef) -> timer:cancel(TRef) end, maps:values(State)).

%%%-----------------------------------------------------------------------------
%%% API EXPORTS
%%%-----------------------------------------------------------------------------
start_interval(Name, Rps) ->
    gen_server:call(?MODULE, {start_interval, Name, Rps}).

stop_interval(Name) ->
    gen_server:call(?MODULE, {stop_interval, Name}).

rps(Name, Rps) ->
    gen_server:call(?MODULE, {rps, Name, Rps}).

%%%-----------------------------------------------------------------------------
%%% INTERVAL EXPORTS
%%%-----------------------------------------------------------------------------
renew_interval(Name) ->
    gen_server:call(?MODULE, {renew_interval, Name}).

%%%-----------------------------------------------------------------------------
%%% GENSERVER EXPORTS
%%%-----------------------------------------------------------------------------
handle_call({start_interval, Name, Rps}, _From, State) ->
    {ok, TRef} = new_interval(Name, Rps),
    {reply, ok, maps:put(Name, TRef, State)};
handle_call({stop_interval, Name}, _From, State) ->
    case maps:get(Name, State, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TRef ->
            timer:cancel(TRef),
            {reply, ok, maps:remove(Name, State)}
    end;
handle_call({rps, Name, Rps}, _From, State) ->
    case nthrottle_tab:rps(Name) of
        {ok, _OldRps} ->
            nthrottle_tab:rps(Name, Rps),
            case update_interval(Name, State) of
                {ok, TRef} ->
                    {reply, ok, maps:put(Name, TRef, State)};
                {error, not_found} ->
                    {reply, {error, not_found}, State}
            end;
        Otherwise ->
            {reply, Otherwise, State}
    end;
handle_call({renew_interval, Name}, _From, State) ->
    case update_interval(Name, State) of
        {ok, TRef} ->
            {reply, ok, maps:put(Name, TRef, State)};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end.

handle_cast(Msg, State) ->
    erlang:error(function_clause, [Msg, State]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
new_interval(Name, Rps) ->
    {_Max, Time} = nthrottle_util:from_rps(Rps),
    timer:apply_after(Time, ?MODULE, renew_interval, [Name]).

update_interval(Name, State) ->
    case maps:get(Name, State, undefined) of
        undefined ->
            {error, not_found};
        OldTRef ->
            timer:cancel(OldTRef),
            {ok, Rps} = nthrottle_tab:rps(Name),
            {Max, _Time} = nthrottle_util:from_rps(Rps),
            Subscribers = nthrottle_tab:subscribers(Name, Max),
            nthrottle_util:notify(Subscribers),
            Triggered = erlang:length(Subscribers),
            Triggers = nthrottle_util:safe_substract(Max, Triggered),
            nthrottle_tab:triggers(Name, Triggers),
            new_interval(Name, Rps)
    end.
