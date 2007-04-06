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

void store_d(int *sdp __attribute__((sos_release)), int* store __attribute__((sos_store))) {
    store = sdp;
}

void bad_store_a(int *fap __attribute__((sos_release))) {
    fap++;
}

void no_release(int *fap) {
    fap++;
}


int main() {

    int *pa;
    int *pb;
    int *pc;
    int *pd;
    int *fa;
    int *f;

    pa = malloc(3 * sizeof(int));
    pb = malloc(1 * sizeof(int));
    pc = malloc(4 * sizeof(int));
    pd = malloc(1 * sizeof(int));
    fa = malloc(5 * sizeof(int));
    f  = malloc(9 * sizeof(int));

ONE:

    store_a(pa);
    store_b(pb);
    store_c(pc);
    store_d(pd, store);
    no_release(fa);
    f++;
    
    return 0;
}


