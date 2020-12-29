x = 2;
y = &x;
z = &y;
a = 3;
*z = &a;
**z = 5;
return a + *y;