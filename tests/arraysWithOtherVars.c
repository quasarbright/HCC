int x = 12;
int[5] xs;
int[6] ys = {1,2};
ys[5] = 3;
int y = 32;
return x + xs[4] + ys[0] + ys[1] + ys[2] + ys[5] + y;
//    12 + 0     + 1     + 2     + 0     + 3     + 32