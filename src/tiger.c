#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

int64_t tigermain(void);

int main(void) {
    tigermain();
    return 0;
}

int64_t printi(int64_t value) {
    printf("%"PRId64, value);
    return 0;
}

int64_t make_array(int64_t size, int64_t init) {
    int64_t *array = calloc(size, 8);
    int64_t i = 0;

    for (i = 0; i < size; ++i) {
        array[i] = init;
    }

    return (int64_t) array;
}
