Erlang Property Lists (epl)
===========================

Property-lists support functions.  Before the adoption of the `maps` library and syntax, `proplists` were the closest means to expressing a JSON like structure or Erlang's decoded ASN.1 objects (pain in the arse to work with).


Data Types
----------

* Plist           :: proplists:proplist()
* Path            :: [atom() | pos_integer()]
* Value           :: term()
* Default         :: term()
* PathsValues     :: [{Path, Value}]
* Map             :: map()


Exports
-------

### epl:get_path(Plist0, Path) -> Value | undefined

Given a `Path` into `Plist0`, return the value found; otherwise undefined if not found.

- - -
### epl:get_path(Plist0, Path, Default) -> Value | Default

Given a `Path` into `Plist0`, return the value found; otherwise a `Default` value if not found.

- - -
### epl:is_plist(Thing) -> true | false

Return true if `Thing` adheres to a formal definition of a Plist (suitable to convert to JSON), where each list element is either an atom (short hand of `{Atom, true}`) or 2-tuple Key-Value.

- - -
### epl:set_path(Plist0, Path, Value) -> Plist1

Given a `Path` into `Plist0`, insert or replace a value.  If the `Path` specified does not exist, it will be created.

- - -
### epl:set_paths(Plist0, PathsValues) -> Plist1

Given a list of paths and values, insert or replace one or more values in `Plist0`.  See `set_path/3`.

- - -
### epl:to_map(Plist) -> Map

Convert the nested `Plist` into a nested `Map`.

- - -
### epl:from_map(Map) -> Plist

Convert the nested `Map` into a nested `Plist`.


Copyright
---------

Copyright 2021, 2022 by Anthony Howe.  All rights reserved.


MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
