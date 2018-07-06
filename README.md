Erlang bindings for ChakraCore
===

Yep.


ToDo
---

* Add script names
* Add erlang.send? Or some other callback communication?

OS X Build Notes
---

For some reason Chakra wants to link against icu4c 58 where as the obvious
`brew install chakra` installs icu4c 61 in `/usr/local/opt/icu4c`. I fixed
this by re-linking `/usr/local/opt/icu4c` against
`/usr/local/Cellar/icu4c/58.2`.