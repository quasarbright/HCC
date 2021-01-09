int n = 5;
int i = 2;
int nprev = 1;
int nprevprev = 1;
// without comparison, this only works for n >= 2
while(n + -i) { // TODO integer comparison
    int tmp = nprev + nprevprev;
    nprevprev = nprev;
    nprev = tmp;
    i = i + 1;
    return 1;
}
return nprev;