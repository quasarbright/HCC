# features
* [ ] ifs
* [ ] whiles
    * ~~lift declarations out! otherwise, you'll use O(n) stack slots for the same variable!~~
    * no you won't. The body is compiled once, but runs many times.
    The compiler will only allocate one space for it,
    just like a variable in an if.
    Only thing is it'll get zeroed out every loop.
* [ ] functions
    * path return checking
    * unreachable statement checking
    * jump to cleanup from anywhere
    * statements get checked, not inferred
# general
* guarantee always return
* block checking, not inferring (+ paramorphism)
* (asm) comment rest of compiler
* disallow array assignment