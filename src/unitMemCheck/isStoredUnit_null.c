#include <stdlib.h>

const int NULL = 0;

void * ker_malloc(int size, int id) {
    return malloc(size);
}


void *ker_msg_take_data(int id, void *buff) {    
    void *ret;
    ret = ker_malloc(42, id);
    if (ret == NULL) {
        return NULL;
    }
    memcpy(ret, buff, 42);
    return ret;
}


