Erlang bindings for ChakraCore
===

Yep.


ToDo
---

* Add parseable scripts?
* Add ref counting to contexts
* Add script names
* Add "dot.ed.name" syntax to chakra:call
* Add function resources so we can return a function that can be invoked
* Add erlang.send? Or some other callback communication?
* Raise errors and return unwrapped?

Maybe ToDo?
---

* Shareable runtimes?

OS X Build Notes
---

For some reason Chakra wants to link against icu4c 58 where as the obvious
`brew install chakra` installs icu4c 61 in `/usr/local/opt/icu4c`. I fixed
this by re-linking `/usr/local/opt/icu4c` against
`/usr/local/Cellar/icu4c/58.2`.