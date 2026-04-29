#include <stdio.h>
#include <stdlib.h>

int print_int(int value) {
    printf("%d\n", value);
    return value;
}

int put_char(int value) {
    putchar(value);
    return value;
}