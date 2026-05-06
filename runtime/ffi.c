#include <stdio.h>
#include <stdlib.h>

int print_int(int value) {
    printf("%d\n", value);
    return value;
}

int print_int_inline(int value) {
    printf("%d", value);
    return value;
}

int print_usize_inline(size_t value) {
    printf("%zu", value);
    return 0;
}

int print_string_inline(char *value) {
    printf("%s", value);
    return 0;
}

char *int_to_string(int value) {
    char *buffer = malloc(32);
    snprintf(buffer, 32, "%d", value);
    return buffer;
}

int put_char(int value) {
    putchar(value);
    return value;
}
