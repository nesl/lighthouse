#include <stdio.h>


typedef unsigned char uint8_t;
typedef char int8_t;
typedef unsigned int uint32_t;

const uint8_t BLINK_TID = 0;
const uint32_t BLINK_TIMER_INTERVAL = 1024;

typedef struct {
    uint8_t type;
    uint8_t did;
} Message;

uint8_t pid;
uint8_t blink_state;

extern void sys_timer_start(uint8_t tid, uint32_t interval, uint8_t flags);
extern void sys_timer_stop(uint8_t tid);

static int8_t blink_msg_handler(Message *msg)
{
    switch (msg->type){
        case 1:
            {
                pid = msg->did;
                blink_state = 0;
                printf("LED_RED_TOGGLE\n");
                sys_timer_start(BLINK_TID, BLINK_TIMER_INTERVAL, 1);
                break;
            }


        case 2:
            {
                sys_timer_stop(BLINK_TID);
                printf("Blink Stop\n");
                break;
            }

        case 3:
            {
                printf("LED_GREEN_TOGGLE");
                break;
            }

        default:
            return -1;
    }

    return 0;
}

