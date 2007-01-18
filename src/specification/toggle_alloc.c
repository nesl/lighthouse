#include <stdlib.h>

void *global_buff;

void create_a() {
    global_buff = malloc(10);
    return;
};

void delete_a() {
    free(global_buff);
    return;
};

void create_b() {
    global_buff = malloc(10);
    return;
};

void delete_b() {
    free(global_buff);
    return;
};


