#include <stdio.h>
#include <stdlib.h>
#include <time.h>

const int NULL = 0;
const int stderr = 1;

enum {
    INIT = 0,
    FINAL = 1,
    CREATE = 2,
    DESTROY = 3,
};


char *state;

void init();
void final();
void create();
void destroy();

int main() {

    int seed;
    seed = time(NULL);
    printf("Using seed: %d\n", seed);
    srandom(seed);

    state = NULL;

    for (;;) {
    
        switch (random() % 4) {

            case INIT: 
                init();
                break;

            case FINAL:
                final();
                return 0;
                break;


            case CREATE:
                create();
                break;

            case DESTROY:
                destroy();
                break;

            default:
                fprintf(stderr, "Oops.\n");
                return -1;
        }
    }

    return -1;
}


void init() {
    printf("Init\n");
    return;
}


void final() {
    printf("Final\n");
    return;
}


void create() {
    printf("Create\n");
    state = (char *) malloc(100 * sizeof(char));
    return;
}


void destroy() {
    printf("Destroy\n");
    free(state);
    return;
}


