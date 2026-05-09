#include <stdio.h>
#include <stdlib.h>

char *read_line(void) {
    char *buffer = malloc(1024);
    if (fgets(buffer, 1024, stdin) == NULL) {
        buffer[0] = '\0';
    }
    return buffer;
}

int char_at(char *value, int index) {
    return value[index];
}

int print_int(int value) {
    printf("%d\n", value);
    return value;
}

int print_int_inline(int value) {
    printf("%d", value);
    return value;
}

int add_ints(int left, int right) {
    return left + right;
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
