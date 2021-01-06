int x = 2;
int *y = &x;
int **z = &y;
int a = 3;
*z = &a;
**z = 5;
return a + *y;