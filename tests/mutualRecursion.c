int even(int n);
int odd(int n);

int even(int n) {
    if(n == 0) {
        return 1;
    } else {
        return odd(n + -1);
    }
}

int odd(int n) {
    if(n == 0) {
        return 0;
    } else {
        return even(n + -1);
    }
}

int main() {
    return odd(11) + - even(13);
}