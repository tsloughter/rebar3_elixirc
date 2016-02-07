rebar3_elixirc
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:


```
{provider_hooks, [{pre, [{app_compile, {ex, compile}}]}]}.
{plugins, [rebar3_elixirc]}.

{ex_opts, #{docs => false, ignore_module_conflict => true,
            debug_info => true, warnings_as_errors => false}}.
```

Then just call your plugin directly in an existing application:

```
$ rebar3 ex compile
===> Fetching rebar3_elixirc
===> Compiling rebar3_elixirc
<Plugin Output>
```
