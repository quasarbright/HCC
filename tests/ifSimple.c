int main(){
int x = 1;
if(x) {
    x = x + 1;
}
if(0) {
    x = 0;
} else {
    x = x + 1;
}
return x;}