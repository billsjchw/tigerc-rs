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

uint64_t tiger_strint(uint64_t addr, int64_t value) {
    uint64_t str_len = *(uint64_t *) addr;
    void *buffer = malloc(str_len + 15);
    char *int_str = (char *) buffer + 8 + str_len;
    uint64_t int_str_len = 0;

    if (buffer == NULL) {
        exit(1);
    }

    memcpy(buffer + 8, (void *) addr + 8, str_len);
    sprintf(int_str, "%"PRId64, value);
    int_str_len = strlen(int_str);
    *(uint64_t *) buffer = str_len + int_str_len;

    return (uint64_t) buffer;
}

uint64_t tiger_intstr(uint64_t addr, int64_t value) {
    uint64_t str_len = *(uint64_t *) addr;
    void *buffer = malloc(str_len + 15);
    char *int_str = (char *) buffer + 8;
    uint64_t int_str_len = 0;

    if (buffer == NULL) {
        exit(1);
    }

    sprintf(int_str, "%"PRId64, value);
    int_str_len = strlen(int_str);
    memcpy(buffer + 8 + int_str_len, (void *) addr + 8, str_len);
    *(uint64_t *) buffer = str_len + int_str_len;

    return (uint64_t) buffer;
}
