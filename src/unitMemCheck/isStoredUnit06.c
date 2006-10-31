#include <stdlib.h>

typedef unsigned char uint8_t;


void* ker_malloc(int size, int id) {
    return malloc(size);
}

struct __anonstruct_ads7828_state_t_35 {
   uint8_t calling_module ;
   uint8_t *command ;
   uint8_t channel ;
};

typedef struct __anonstruct_ads7828_state_t_35 ads7828_state_t;

//ads7828_state_t *s;

int main() {

    ads7828_state_t *s  __attribute__((__sos_store__));
    unsigned int __cil_tmp45 ;
    unsigned int __cil_tmp46 ;

    __cil_tmp45 = (unsigned int )s;
    __cil_tmp46 = __cil_tmp45 + 4;
    (*((uint8_t **)__cil_tmp46)) = (uint8_t *)ker_malloc(1U, (unsigned char)129);

    return 0;
}

