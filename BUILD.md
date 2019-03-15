## Toolchain

1.  For pure Haskell libraries:

    1. [stack](https://haskellstack.org)

2.  For external C++ components

    1. [CMake](https://cmake.org)


## Everything except `swarm`

Everything except `swarm` is pure Haskell. So build it with

    $ stack build <component name>


## `swarm`

1.  Build external C++ components out-of-tree

    $ (cd _build && cmake .. && make)

2.  Build `swarm`

    $ stack build swarm
