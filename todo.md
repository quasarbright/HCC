# features
* [x] ifs
* [x] whiles
    * ~~lift declarations out! otherwise, you'll use O(n) stack slots for the same variable!~~
    * no you won't. The body is compiled once, but runs many times.
    The compiler will only allocate one space for it,
    just like a variable in an if.
    Only thing is it'll get zeroed out every loop.
* [ ] functions
    * path return checking
    * unreachable statement checking
    * jump to cleanup label on return
    * statements get checked, not inferred
    * main function gets called?
    * no linking with main.c?
    * include print int function? (can use syscall)
* [ ] other primitive types, 32-bit ints
    * tough bc of value sizes
    * can't just make things 1 word bc strings
     and signed 32-bit ints will be busted
    * need to use different register sizes
    * need to make size specifications on instructions
    * need to keep track of sizes of types and variables (arrays)
* [ ] sizeof
* [ ] casting
* [ ] structs
* [ ] malloc
# general
* implement other operations (-/++-- etc.)
* guarantee always return, check unreachable statements
* block checking, not inferring (+ paramorphism)
* (asm) comment rest of compiler
* disallow array assignment
* add assignment expressions and make
a for-loop's update be an expression, not a statement
    * at least make it so you don't need the trailing semicolon!
* [x] use writer monad in compiler!
* fix array addressing (they go high to low)