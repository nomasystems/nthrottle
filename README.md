# nthrottle
![nthrottle](https://github.com/nomasystems/nthrottle/actions/workflows/build.yml/badge.svg)

`nthrottle` is an Erlang application that provides a simple API to implement a highly performant Throttling pattern in your BEAM apps.

## Installation
Add `nthrottle` to your project dependencies.

```erl
%%% e.g., rebar.config
{deps, [
    {nthrottle, {git, "git@github.com:nomasystems/nthrottle.git", {tag, "1.0.0"}}}
]}.
```

Add `nthrottle` to your `app.src` applications section.

## Example of usage

Once you have the `nthrottle` application running you can add different throttling instances identified by `Name`:

```erl
%% Start a throttling instance with rps set to infinity (no throttling active)
1> nthrottle:start_throttling('test_name', infinity).
ok

%% Retrieve current rps:
2> nthrottle:rps('test_name').
{ok,infinity}

%% In order for your consumers to use the throttling call the function nthrottle:throttle/1
3> nthrottle:throttle('test_name').
ok

%% Change the current rps to 1
4> nthrottle:rps('test_name', 1).
ok

%% Now if you consume more of once per second then you will receive the atom rps_exceeded
5> nthrottle:throttle('test_name').
ok
6> nthrottle:throttle('test_name').
rps_exceeded

%% Stop your throttling instance
7> nthrottle:stop_throttling('test_name').
ok

%% Now if you try to consume you will receive a bad argument error exception
13> nthrottle:throttle('test_name').
** exception error: bad argument
```
