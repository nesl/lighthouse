#include <stdlib.h>

void free(void*);

void foo(int *p) {
    p++;
}


// Set up data for testing!!!
int main() {

    int* safe_a;
    int* safe_b;
    int* unsafe_a;
    int* unsafe_b;
    int* unsafe_c;
    int* unsafe_d;
    int* unsafe_e;
    int* unsafe_f;
    int* unsafe_g;

    int tmp_val;
    int* tmp_ptr;
    
    // Allocate data
    safe_a = malloc(10 * sizeof(int));
    safe_b = malloc(10 * sizeof(int));
    unsafe_a = malloc(10 * sizeof(int));
    unsafe_b = malloc(10 * sizeof(int));
    unsafe_c = malloc(10 * sizeof(int));
    unsafe_d = malloc(10 * sizeof(int));
    unsafe_e = malloc(10 * sizeof(int));
    unsafe_f = malloc(10 * sizeof(int));
    unsafe_g = malloc(10 * sizeof(int));

    // Use data
    safe_a[3] = 1;
    safe_b[4] = 1;
    unsafe_a[2] = 7;
    unsafe_b[1] = 8;
    unsafe_c[2] = 8;
    unsafe_d[1] = 8;
    unsafe_e[2] = 8;
    unsafe_f[4] = 5;
    unsafe_g[9] = 0;

    // Free data
    free(safe_a);
    free(safe_b);
    free(unsafe_a);
    free(unsafe_b);
    free(unsafe_c);
    free(unsafe_d);
    free(unsafe_e);
    free(unsafe_f);
    free(unsafe_g);

    // From this point on all of safe_* and unsafe_* should be treated as
    // dead. 
ONE:

    // safe_a is okay since it is not refered to
    
    // This is okay since safe is not dereferenced
    safe_b = 0;

    // This is NOT safe since unsafe_a must be dereferenced to get to the
    // field specified by [1]
    unsafe_a[9] = 8; 

    // NOT safe since it is not okay to create aliases to the data that (was)
    // pointed to by unsafe_b.
    tmp_ptr = unsafe_b;

    // NOT safe since we can not derefence unsafe_c
    tmp_val = unsafe_c[2];

    // NOT safe
    if(unsafe_d[3] > 3) {
        tmp_val = 5;
    }

    // NOT safe
    foo(unsafe_e);
   
    // Also NOT safe twice!
    if(tmp_val < 5) {
        tmp_val = unsafe_f[4];
    } else {
        tmp_val = unsafe_g[5];
    }
    
    return 0;
}


