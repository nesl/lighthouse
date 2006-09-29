
void* __attribute__((sos_claim)) sys_malloc(int size);
void sys_free(void *ptr __attribute__((sos_release)));
int *state __attribute__((sos_store));

