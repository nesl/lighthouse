#include <stdlib.h>

int* createIntArray(int size) {
    return (malloc(sizeof(int) * size));
}

void freeArray(int* buffer) {
    free(buffer);
}

void setArray(int *buffer, int size) {
    int i = 0;

    for(i=0; i<42; i++) {
        array[i] = i;
    }


}

int main() {

    int *array;
    int size;
    int i;
    
    size = 42;

    array = createIntArray(size);

    for(i=0; i<42; i++) {
        array[i] = i;
    }

    freeArray(array);
    
    return 0;
}
