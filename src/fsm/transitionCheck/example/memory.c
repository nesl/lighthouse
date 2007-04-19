/* -*- Mode: C; tab-width:4 -*- */
/* ex: set ts=4 shiftwidth=4 softtabstop=4 cindent: */

/**
 * Simple module for examining state diagrams
 */

#include <module.h>
#define LED_DEBUG
#include <led_dbg.h>

#define MEMORY_TID 1

/**
 * Minimal module state
 */
typedef struct {
    uint8_t pid;
    uint8_t state;
    uint8_t *buffer;
} app_state_t;


static int8_t memory_msg_handler(void *start, Message *e);


/**
 * This is the only global variable one can have.
 */
static mod_header_t mod_header SOS_MODULE_HEADER = {
    .mod_id         = DFLT_APP_ID0,
    .state_size     = sizeof(app_state_t),
    .num_timers     = 1,
    .num_sub_func   = 0,
    .num_prov_func  = 0,
    .platform_type  = HW_TYPE /* or PLATFORM_ANY */,
    .processor_type = MCU_TYPE,
    .code_id        = ehtons(DFLT_APP_ID0),
    .module_handler = memory_msg_handler,
};


static int8_t memory_msg_handler(void *state, Message *msg)
{
    app_state_t *s = (app_state_t*)state;

    /** Switch to the correct message handler
     *
     * This module handles three types of messages:
     *
     * \li MSG_INIT to start a timer and allocated a buffer
     *
     * \li MSG_TIMER_TIMEOUT to periodicly write a value to the buffer.  Note
     * that we are expecting that the timer will have timer with ID equal to
     * MEMORY_TID
     *
     * \li MSG_FINAL to stop the timer and deallocate the buffer
     *
     * This module alternates between owning an alocated buffer (state = 1)
     * and having released this buffer (state = 0).  It is important that
     * these stats do not get mixed up :-)
     *
     */
    switch (msg->type){

        case MSG_INIT:
            {
                // I'm alive!
                LED_DBG(LED_GREEN_TOGGLE);

                s->pid = msg->did;
                s->state = 0;

                ker_timer_init(s->pid, MEMORY_TID, TIMER_REPEAT);
                ker_timer_start(s->pid, MEMORY_TID, 1000);
                break;
            }


        case MSG_FINAL:
            {
                // Bye bye!
                ker_timer_stop(s->pid, MEMORY_TID);
                
                if (s->state != 0) {
                    ker_free(s->buffer);
                } else {
                    // Do nothing!
                }
                
                break;
            }


        case MSG_TIMER_TIMEOUT:
            {
                MsgParam* params = (MsgParam*)(msg->data);

                if (params->byte == MEMORY_TID)
                {

                    // Blink the LED
                    if (s->state == 0) {
                        s->buffer = (uint8_t *) ker_malloc(sizeof(uint8_t), s->pid);
                        *(s->buffer) = 0x42;
                        LED_DBG(LED_YELLOW_OFF);
                    } else {
                        ker_free(s->buffer);
                        LED_DBG(LED_YELLOW_ON);
                    }
                    
                    // Update the state
                    s->state++;
                    if (s->state > 1) {
                        s->state = 0;
                    }

                }

                break;
            }

        default:
            return -EINVAL;
    }

    return SOS_OK;
}

#ifndef _MODULE_
mod_header_ptr memory_get_header()
{
    return sos_get_header_address(mod_header);
}
#endif

