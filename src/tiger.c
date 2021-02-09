#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

int64_t tiger_main(void);

int main(void) {
    tiger_main();
    return 0;
}

int64_t tiger_print(uint64_t addr) {
    uint64_t len = *(uint64_t *) addr;
    char *str = (char *) addr + 8;
    uint64_t i = 0;

    for (i = 0; i < len; ++i) {
        putchar(str[i]);
    }

    return 0;
}

int64_t tiger_printi(int64_t value) {
    printf("%"PRId64, value);
    return 0;
}

uint64_t tiger_make_array(uint64_t size, int64_t init) {
    int64_t *array = calloc(size, 8);
    uint64_t i = 0;

    if (array == NULL) {
        exit(1);
    }

    for (i = 0; i < size; ++i) {
        array[i] = init;
    }

    return (uint64_t) array;
}

uint64_t tiger_malloc(uint64_t size) {
    void *ptr = malloc(size);

    if (ptr == NULL) {
        exit(1);
    }

    return (uint64_t) ptr;
}
