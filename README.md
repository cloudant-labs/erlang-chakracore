Erlang bindings for ChakraCore
===

This is a set of thin wrappers around Microsoft's ChakraCore JavaScript engine. Currently these bindings allow for the basic interaction of Erlang and JavaScript via script evaluation and calling named functions. There is basic support for converting a subset of Erlang terms (roughly the ErlJSON representation used by Jiffy) to JavaScript and back.

Basic Terminology
---

When using `erlang-chakracore` there are two main concepts that need to be handled by the developer to ensure efficient use of the ChakraCore engine.

1. `Runtime` - provides the execution environment which includes memory limits, garbage collection, and so on
2. `Context` - provides independent execution environments for JavaScript code (i.e., the global object and persistent namespace across JS invocations)

There can be more than a single `Runtime` active in the Erlang VM at a time. A `Runtime` can also have multiple associated `Context`s. There can only ever be one active `Context` on a `Runtime` at any given point in time. This is managed by `erlang-chakracore` by requiring all `Runtime` and associated `Context`s to only ever be used on the same Erlang process that created them. A `Context` may not be used on any other `Runtime` than the one on which it was created.

In general there's a balance to find between the relatively more heavy `Runtime` and the lighter weight `Context` instances that are more disposable. This balance will depend on your particular use case.

Thus, if a user desires concurrent JavaScript execution in the Erlang VM they must instantiate separate `Runtime` and `Context` instances and manage the JavaScript execution between PIDs.

Examples
---

### Kick the Tires Test

```erlang
{ok, Ctx} = chakra:create_context(),
{ok, 2} = chakra:eval(Ctx, <<"function a(v) {return v * 2;}; a(1);">>),
{ok, 4} = chakra:call(Ctx, a, [2]).
```

This basic example shows a couple of the concepts that were previously discussed. A `Context` is created (a `Runtime` is implicitly created for this context, more on this shortly), some script is evaluated returning a result, and then a second call into a named function is made showing the persistent name space of a context.

Also note that the basic Erlang terms that can be round tripped through JavaScript are also shown.

### Shared Runtime

```erlang
{ok, Rt} = chakra:create_runtime(),
{ok, Ctx1} = chakra:create_context(Rt),
{ok, Ctx2} = chakra:create_context(Rt),
{ok, 2} = chakra:eval(Ctx1, <<"function a(v) {return v * 2;}; a(1);">>),
{error, undefined_function} = chakra:call(Ctx2, a, [2]).
```

Similar to the previous example except we can see here that the separate contexts do not share a common namespace. If you want to create multiple contexts that share a `Runtime` you simply need to create one and pass it to each invocation of `chakra:create_context/1`.

### Interrupt JS Execution

```erlang
{ok, Rt} = chakra:create_runtime([allow_script_interrupt]),
{ok, Ctx} = chakra:create_context(Rt),
spawn(fun() ->
    timer:sleep(2000),
    chakra:interrupt(Rt)
end),
{error, in_disabled_state} = chakra:eval(Ctx, <<"while(true) {};">>),
{error, in_disabled_state} = chakra:eval(Ctx, <<"var a = 1; a * 5;">>),
ok = chakra:enable(Rt),
{ok, 5} = chakra:eval(Ctx, <<"var a = 1; a * 5;">>).
```

If a developer intends to allow for execution of arbitrary user supplied JavaScript its a good idea to set up a watchdog for every runtime. This watchdog is a separate Erlang pid that will call `chakra:interrupt/1` with the given `Runtime` instance at the desired timeout. Note that the `allow_script_interrupt` option must be provided when creating the `Runtime`.

When a `Runtime` is interrupted it is left in the `disabled` state which prevents it from executing any JavaScript until it is re-enabled via `chakra:enable/1`.

*Note*: `chakra:interrupt/1` and `chakra:memory_usage/1` are the only two functions that can be called from any process that did not create the `Runtime` instance.


### Script Serialization

```erlang
{ok, Rt} = chakra:create_runtime(),
{ok, Ctx} = chakra:create_context(Rt),
timer:tc(fun() ->
    lists:foreach(fun(_) ->
        {ok, 8} = chakra:eval(Ctx, <<"var a = 2; a * 4;">>)
    end, lists:seq(1, 10000))
end).
{ok, NewRt} = chakra:create_runtime(),
{ok, NewCtx} = chakra:create_context(NewRt),
timer:tc(fun() ->
    {ok, Script} = chakra:serialize(Ctx, <<"var a = 2; a * 4;">>),
    lists:foreach(fun(_) ->
        {ok, 8} = chakra:run(NewCtx, Script)
    end, lists:seq(1, 10000))
end).
```

In this (rather contrived) example we see that a script can be serialized. That is, it can be parsed and have its AST representation pre-prepared before running. On my machine this totally unscientific and horrible benchmark shows about a 5x difference between the two. Also, note that the serialized version can be run on any `Runtime` in any `Context`.

*Note*: Under the hood, `chakra:eval/2` is merely a convenience function around `chakra:serialize/2` and `chakra:run/2`.


API Notes
---

A short collection of non-obvious things the API is capable of along with explanations of some of ChakraCore's available options.

### Runtime Options

* `{memory_limit, integer()}` - Limit the total amount of memory for a given `Runtime` instance. Once this limit is reached ChakraCore will start generating "Out of memory" errors until garbage can be collected.
* `disable_background_work` - By default, ChakraCore creates two threads per `Runtime` instance for garbage collection and other background tasks. This option can disable the use of extra threads and that work will be done on the same thread as the `Runtime` is executing on. This is advisable if a large number of `Runtime` instances are created simultaneously.
* `allow_script_interrupt` - Allows the use of `chakra:interrupt/1` from other Erlang processes
* `enable_idle_processing` - An advanced feature allows developers to inform the JavaScript engine when its appropriate for a `Context` to perform its idle work via `chakra:idle/1`. (I am unsure about the utility of this approach)
* `disable_native_code_generation` - Disable ChakraCore's JIT
* `disable_eval` - Disable the ability of JavaScript code to call `eval`.
* `enable_experimental_features` - Will enable experimental features in whatever version of ChakraCore you have linked to (i.e., super advanced usage for those that know their specific ChakraCore build/deployment targets).


### Eval/Run Options

* `{source_url, binary()}` - Provide a name for the script that is evaluated. This is how you can control source code name and location information in exception stack traces. While its called `source_url` there's no requirement that it be an actual URL. Anything like `foo.js` will work just fine here.


### `chakra:gc/1`

You can selectively force a garbage collection on a runtime. This is likely only useful for runtimes created with the `disable_background_work` option when there's a large number of contexts running on the same `Runtime`.


### `chakra:enable/1`/`chakra:disable/1`

As discussed earlier, `chakra:enable/1` is required after a `Runtime` instance is interrupted. There's a corresponding `chakra:disable/1` for parity though I have not thought of any place where it would be useful.


ToDo
---

* Support message passing from JavaScript to Erlang?


OS X Build Notes
---

I've found a number of bugs in ChakraCore that adversely affect its use within the Erlang VM. For the time being I recommend that users of erlang-chakracore clone `https://github.com/Microsoft/ChakraCore` and build locally.

On OS X this can be done fairly easily by doing:

```sh
$ brew install icu cmake
$ ./build.sh --icu=/usr/local/opt/icu4c/include/ --extra-defines=U_USING_ICU_NAMESPACE=1 --lto-thin --libs-only -j=4 -y
$ cp out/Release/libChakraCore.dylib /usr/local/lib/
$ cp out/Release/include/* /usr/local/include/
```