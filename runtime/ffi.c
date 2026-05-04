#include <stdio.h>
#include <stdlib.h>

int print_int(int value) {
    printf("%d\n", value);
    return value;
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
