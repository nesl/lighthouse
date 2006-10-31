#include <stdlib.h>

typedef unsigned char uint8_t;

uint8_t ker_change_own (void* ptr, int id) {
    ptr = malloc(42);   
    return 0;
}


uint8_t post_net(int did, int sid, uint8_t type, uint8_t arg, void *larg, int flag, int daddr) {
    free(larg);
    return 0;
}


int main () {

    uint8_t *pktpayload;

    ker_change_own((void *)pktpayload, 32);

    return post_net(32, 32,
            128, 42, pktpayload,
            0x04, 0xFFFF);
}
