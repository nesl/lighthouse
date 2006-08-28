#include <stdlib.h>

const int MAX_SIZE = 100;

void kill(int x) {
    x = x + 1;
}

int main() {

        int x;
        int y;
        int z;

        int *y_ptr;
        int **y_ptr_ptr;
        
        int *alias_ptr;
        int **alias_ptr_ptr;
       
        ////
        // Setup
        ////
       
        x = 4; 
        z = 2;
        y = z;
        y_ptr = &y;
       
        ////
        // Create base aliases
        ////

        alias_ptr = y_ptr;
        // (y, z) (y_ptr, &y, alias_ptr)
        
        y_ptr_ptr = &y_ptr;
        alias_ptr_ptr = &alias_ptr;
        // (y, z) (y_ptr, &y, alias_ptr) (y_ptr_ptr, &y_ptr) (&alias_ptr, alias_ptr_ptr)

        *alias_ptr = x;
        // (z) (y_ptr, &y, alias_ptr) (y_ptr_ptr, &y_ptr) (&alias_ptr, alias_ptr_ptr)
        // (*alias_ptr, x)
        
        //kill(y);
        
CHECK:

    return 0;
}

