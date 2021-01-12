int main(){
int x = 2;
int *y = &x;
int *z = &*y;
*z = 3;
return x;}