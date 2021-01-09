/*
variables are locally scoped according to static analysis,
but the compiler doesn't consider a variable's stack slot free
once it falls out of scope.
This is necessary for addrOf to work properly.
*/
int x = 7;
int *y;
if(1) {
    int z = 11;
    *y = &z;
}
int a = 29; // shouldn't trash z's stack slot. should use a new one
return *y;