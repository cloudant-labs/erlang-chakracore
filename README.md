Erlang bindings for ChakraCore
===

Yep.


ToDo
---

* Add ERL_NIF_TERM <-> JsValueRef conversions
* Add chakra:call(Ctx, FunName, Args) -> Result
* Bind context to process
* Add runtime options
  * Add resource limits
  * Add resource tracking?
  * Add script interrupt? Per call?
* Add erlang.send? Or some other callback communication?


Maybe ToDo?
---

* Shareable runtimes?
* Pre-define atoms for JsErrorCode -> ERL_NIF_TERM conversion

OS X Build Notes
---

For some reason Chakra wants to link against icu4c 58 where as the obvious
`brew install chakra` installs icu4c 61 in `/usr/local/opt/icu4c`. I fixed
this by re-linking `/usr/local/opt/icu4c` against
`/usr/local/Cellar/icu4c/58.2`.