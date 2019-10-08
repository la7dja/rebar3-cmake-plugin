rebar3-cmake-plugin
====================

A rebar3 plugin for building C code with CMake

Build
-----

  $ rebar3 compile

Use
---

Add the plugin to your rebar config:

```erlang
{plugins, [
  {cmake, {git, "git://github.com/la7dja/rebar3-cmake-plugin.git", {branch, "master"}}}
]}.
```

Configure cmake options (all values below are defaults, except 'variables'):

```erlang
{cmake_opts,
 [{c_src, "c_src"},
  {c_build, "c_src"},
  {cmake_cmd, "cmake"},
  {make_cmd, "make"},
  {compile_target, ""},
  {clean_target, "clean"},
  {variables, [{"CMAKE_BUILD_TYPE", "Debug"}]}
]}.
```

The `c_src` and `c_build` option values are relative to the app's location.

Add a hook to automatically build C files and clean them afterwards:

```erlang
{provider_hooks, [
  {pre, [
    {compile, {cmake, compile}},
    {clean, {cmake, clean}}
  ]}
]}.
```
