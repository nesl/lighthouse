#include <stdlib.h>

void* ker_malloc(int size, int id) {
    return malloc(size);
}

void * msg_create()
{
    void *tmp;
    tmp = ker_malloc(sizeof(int) * 4, 0);
    if (tmp != 0) {
        tmp[0] = 4;
        tmp[1] = 0;
        return tmp;
    }
    tmp = ker_malloc(8);
    return tmp;
}


