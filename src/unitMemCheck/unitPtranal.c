#include <stdlib.h>

const int MAX_SIZE = 100;

int main() {

    int *arrayA;
    int *arrayAliasA;
    int **arrayPtrA;
    
    int *arrayB;
    int *arrayAliasB;
    int **arrayPtrB;
    
    int **arrayPtrAB;
    
    ////
    // A
    ////
    
    // arrayA can pt to malloc
    arrayA = malloc(sizeof(int) * MAX_SIZE);

    // arrayAliasA can pt to whatever array pts to
    arrayAliasA = arrayA;

    // arrayPtrA can pt to arrayA
    arrayPtrA = &arrayA;
    
    
    ////
    // B
    ////
    
    // arrayB can pt to malloc
    arrayB = malloc(sizeof(int) * MAX_SIZE);

    // arrayAliasB can pt to whatever arrayB pts to
    arrayAliasB = arrayB;

    // arrayPtrB can pt to arrayB
    arrayPtrB = &arrayB;
    

    ////
    // AB
    ////
    
    // arrayPtrAB can pt to arrayA
    arrayPtrAB = &arrayA;

    // arrayPtrAB can ALSO pt to arrayB
    // so arrayPtrAB can point to arrayA OR arrayB
    arrayPtrAB = &arrayB;
    
    return 0;
}

