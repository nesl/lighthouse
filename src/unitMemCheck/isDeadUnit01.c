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

int post_long(int sid, int mid, int did, int data_size, void* data, int flag) {
    ker_free(data);
    return flag;
}

void msg_send_senddone(Message *msg_sent, int succ, int msg_owner)
{
    int flag;


    if(msg_sent->flag) {
        ker_free(msg_sent->data);
    }

    if(post_long(msg_sent->len, msg_owner, 42, sizeof(Message), msg_sent, msg_sent->flag) < 0) {
        ker_free(msg_sent);
    }

    return;
}

