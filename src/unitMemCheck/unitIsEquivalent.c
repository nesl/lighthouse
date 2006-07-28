#include <stdlib.h>

const int MAX_SIZE = 100;

int main() {

    int *arrayA;
    int *arrayCloneA;
    
    int *arrayB;
    int *arrayCloneB;
    
    int *arrayCloneAB;
    
ONE: 
    
    arrayA = malloc(sizeof(int) * MAX_SIZE);
    arrayCloneA = arrayA;
    arrayB = malloc(sizeof(int) * MAX_SIZE);
    arrayCloneB = arrayB;

    // equivalence set {arrayCloneA, arrayA}
    // equivalence set {arrayCloneB, arrayB}
TWO:

    arrayCloneAB = arrayA;
    arrayCloneAB = arrayB;

    // equivalence set {arrayCloneA, arrayA}
    // equivalence set {arrayPtrAB, arrayB, arrayCloneB}
THREE:

    if(arrayA[0] > arrayB[0]) { 
        arrayCloneAB = arrayA; 
    }

    // equivalence set {arrayCloneA, arrayA}
    // equivalence set {arrayCloneB, arrayB}
FOUR:


    arrayCloneA = 0;
    arrayCloneB = 0;
    arrayCloneAB = 0;
FIVE:

    return 0;
}

