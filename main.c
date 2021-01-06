#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>

extern int64_t our_code_starts_here() asm("our_code_starts_here");

int main() {
    int64_t ans = our_code_starts_here();
    printf("%" PRId64 "\n", ans);
    exit(0);
    return 0;
}