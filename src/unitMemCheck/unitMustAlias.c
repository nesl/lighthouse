#include <stdlib.h>

const int MAX_SIZE = 100;

int main() {

    int *arrayA;
    int *arrayAliasA;
    
    int *arrayB;
    int *arrayAliasB;
    
    int *arrayAliasAB;
    
    ////
    // A
    ////
   
    arrayA = malloc(sizeof(int) * MAX_SIZE);
ONE: 

    // arrayAliasA MUST alias arrayA
    arrayAliasA = arrayA;
TWO:

    ////
    // B
    ////

    arrayB = malloc(sizeof(int) * MAX_SIZE);
THREE:

    // arrayAliasB MUST alias arrayB
    arrayAliasB = arrayB;
FOUR:

    
    ////
    // AB
    ////
    
    // arrayPtrAB MUST alias arrayA
    arrayAliasAB = arrayA;
FIVE:

    // arrayPtrAB MUST now alias arrayB
    arrayAliasAB = arrayB;
SIX:

    // This conditional should make it such that arrayAliasAB may alias arrayB
    // OR arra A.  So its MUST alias information becomes ambigous.
    if(arrayA[0] > arrayB[0]) { 
        arrayAliasAB = arrayA; 
    }
SEVEN:

    // Finish off by nulling all of the pointers
    arrayAliasA = 0;
    arrayAliasB = 0;
    arrayAliasAB = 0;
EIGHT:
    
    return 0;
}

