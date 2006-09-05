#include <stdlib.h>

int* store;

void store_a(int *sap __attribute__((sos_release))) {
    store = sap;
}

void store_b(int *sbp __attribute__((sos_release))) {
    store_a(sbp);
}

void store_c(int *scp __attribute__((sos_release))) {
    int *local_store __attribute__((sos_store));
    local_store = scp;
}

void fail_a(int *fap __attribute__((sos_release))) {
    fap++;
}

int main() {

    int *pa;
    int *pb;
    int *pc;
    int *pd;

    pa = malloc(3 * sizeof(int));
    pb = malloc(1 * sizeof(int));
    pc = malloc(4 * sizeof(int));
    pd = malloc(1 * sizeof(int));

ONE:

    store_a(pa);
    store_b(pb);
    fail_a(pc);
    pd++;
    
    return 0;
}


