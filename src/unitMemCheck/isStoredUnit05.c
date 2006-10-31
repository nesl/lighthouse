#include <stdlib.h>

typedef struct {
    int len;
    int *data;
    int flag;
} Message;

static Message store;

void* ker_malloc(int size, int id) {
    return malloc(size);
}

int main() {

    unsigned int tmp;
    tmp = (unsigned int)(&store) + 4; 
    (*((int **)tmp)) = (int*) ker_malloc(42, 1);
    
    return 0;
}

