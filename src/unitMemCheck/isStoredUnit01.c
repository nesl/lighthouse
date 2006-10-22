#include <stdlib.h>

static int *cam_bin[8] = {0};

void* ker_malloc(int size, int id) {
    return malloc(size);
}

char ker_cam_add(int key, void *value)
{
    int *cam; 

    cam = ker_malloc(sizeof(int) * 42, 69);

    if( cam == 0 ) { return -1; }

    cam[0] = 1;
    cam[2] = key;
    cam[3] = value;
    
    cam_bin[key] = cam;
    return 0;  
}

