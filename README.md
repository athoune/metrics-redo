Metrics Redo
============

Redis implementation for [metrics](https://github.com/athoune/metrics) tools.

Build and test
--------------

    ./rebar get-deps
    ./rebar compile
    ./rebar eunit

Example
-------

```erlang
application:start(metrics),
ok = metrics:add_writer(metrics_redis),
metrics_redis:flush(), % clean all datas
metrics_counter:incr(foo, 42),
42 = metrics_redis:value(foo).
```

