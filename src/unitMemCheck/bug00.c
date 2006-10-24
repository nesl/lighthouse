#include <stdlib.h>

typedef struct {
    int len;
    void *data;
    int flag;
} Message;

typedef struct {
    int fieldA;
    int fieldB;
} Data;

void* ker_malloc(int size, int id) {
    return malloc(size);
}

void* ker_msg_take_data(int size, void* data) {
    return malloc(size);
}

void ker_free(void *data) {
    free(data);
    return;
}

int post_net(int sid, 
        int mid, 
        int did, 
        int data_size, 
        void* data, 
        int flag,
        int address) {
    ker_free(data);
    return flag;
}

static int tr_send_data(Data *s, Message *msg)
{
    void *hdr = ker_msg_take_data(42, msg); 
    int dup;
    int my_id = s->fieldB;
    
    if(hdr == 0) return -1;

    if(s->fieldA == 0) {
        if(my_id == 42) {
            post_net(42, 
                    42,
                    69, 
                    msg->len, 
                    msg->data,
                    0x04, 
                    255);
            // MEMORY LEAK HERE.  The above call to post should be realeasing
            // hdr rather than msg->data.
            return 0;
        } else {
            ker_free(hdr);
            return -1;
        }
    }

    if(s->fieldA >= ((int*)hdr)[0] ) {
        ker_free(hdr);
        return -1;
    }

    if(my_id != 18) {
        my_id += 4;
    } else {
        dup = 0;
    }

    if(dup == 0) {
        post_net(42, 
                42,
                69, 
                msg->len, 
                msg->data,
                0x04, 
                s->fieldA);
        return 0;
    } else {
        ker_free(hdr);
        return -1;
    }

}

int main() {
    void* a = malloc(3);
    void* b = malloc(5);
    tr_send_data((Data*)a, (Message*)b);
    free(a);
    free(b);
    return 0;
}
