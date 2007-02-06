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

void bad_a() {
    create_a();
    return;
}

void bad_b() {
    create_a();
    create_a();
    return;
}

void bad_c() {
    return;
}

void bad_d() {
    void *tmp;
    tmp = malloc(5);
    return;
}
