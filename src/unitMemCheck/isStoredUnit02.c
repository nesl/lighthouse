#include <stdlib.h>

static int *cam_bin[8] = {0};

void* ker_malloc(int size, int id) {
    return malloc(size);
}

void foo_bar(int key)
{

    cam_bin[key] = ker_malloc(sizeof(int) * 42, 69);

    return;  
}

