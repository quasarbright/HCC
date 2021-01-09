int x = 10;
int i = 0;
while(x) {
    int y = 20;
    while(y) {
        i = i + 1;
        y = y + -1;
        return 1;
    }
    x = x + -1;
    return 1;
}
return i;
