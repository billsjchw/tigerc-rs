#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

int64_t tigermain(void);

int main(void) {
    tigermain();
    return 0;
}

int64_t printi(int64_t value) {
    printf("%"PRId64, value);
    return 0;
}
