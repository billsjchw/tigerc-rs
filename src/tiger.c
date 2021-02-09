#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

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

int64_t tiger_strcmp(uint64_t lhs_addr, uint64_t rhs_addr) {
    uint64_t lhs_len = *(uint64_t *) lhs_addr;
    uint64_t rhs_len = *(uint64_t *) rhs_addr;
    char *lhs_str = (char *) lhs_addr + 8;
    char *rhs_str = (char *) rhs_addr + 8;
    uint64_t i = 0;

    for (i = 0; i < lhs_len && i < rhs_len; ++i) {
        if (lhs_str[i] < rhs_str[i]) {
            return -1;
        }
        if (lhs_str[i] > rhs_str[i]) {
            return 1;
        }
    }

    if (lhs_len < rhs_len) {
        return -1;
    } else if (lhs_len > rhs_len) {
        return 1;
    } else {
        return 0;
    }
}
