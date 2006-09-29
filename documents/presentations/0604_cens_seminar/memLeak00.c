void* __attribute__((sos_claim)) malloc(int size);
void free(void *ptr __attribute__((sos_release)));

int main() {

    int *p;
    p = malloc(sizeof (int));
    return 0;
}

