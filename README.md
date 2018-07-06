Erlang bindings for ChakraCore
===

Yep.


ToDo
---

* Add "dot.ed.name" syntax to chakra:call
* Add erlang.send? Or some other callback communication?
* Add script names

OS X Build Notes
---

For some reason Chakra wants to link against icu4c 58 where as the obvious
`brew install chakra` installs icu4c 61 in `/usr/local/opt/icu4c`. I fixed
this by re-linking `/usr/local/opt/icu4c` against
`/usr/local/Cellar/icu4c/58.2`.