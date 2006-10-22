#include <stdlib.h>

typedef struct {
    int len;
    void *data;
    int flag;
} Message;


void* ker_malloc(int size, int id) {
    return malloc(size);
}

void ker_free(void *data) {
    free(data);
    return;
}


void msg_send_senddone(Message *msg_sent, int succ, int msg_owner)
{
    int flag;

    if(msg_sent->flag == 0) {
        ker_free(msg_sent);
        return;
    }

    if(msg_sent->flag) {
        ker_free(msg_sent->data);
        msg_sent->flag = 0;
    }

    return;
}

