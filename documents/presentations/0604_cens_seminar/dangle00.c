void* __attribute__((sos_claim)) malloc(int size);
void free(void *ptr __attribute__((sos_release)));

int main() {

    int *p;
    int q;
    
    p = malloc(sizeof (int));
    free(p);

    q = *p;

    return 0;
}

