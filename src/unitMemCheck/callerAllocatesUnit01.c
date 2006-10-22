#include <stdlib.h>

static int *cam_bin[8] = {0};

typedef struct {
    int len;
    void *data;
} Message;

void* ker_malloc(int size, int id) {
    return malloc(size);
}

void* ker_change_own(Message *m, int id) {
    return malloc(m->len);
}

void *ker_msg_take_data(int pid, Message *msg)
{
  int *ret;

  if(pid > 5) {
    ker_change_own((void*)msg->data, pid);
    ret = msg->data;
    msg->len = 0;
    msg->data = 0;
    return ret;
  } else {
    ret = ker_malloc(msg->len, pid);
    if(ret == 0) return 0;
    return ret;
  }
}

