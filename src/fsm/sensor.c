enum {SENSOR_DATA_READY_FID,
    SENSOR_CONTROL_FID,
    SENSOR_GET_DATA_CMD,
    SENSOR_ENABLE_CMD,
    SENSOR_DISABLE_CMD,
    SENSOR_CONFIG_CMD,
    MSG_INIT,
    MSG_FINAL,
};


const void* NULL = 0;
const int ACCEL_0_SENSOR_ID = 1;
const int ACCEL_1_SENSOR_ID = 2;
const int MTS310_ACCEL_0_SID = 3;
const int MTS310_ACCEL_1_SID = 4;
const int SOS_MSG_PAYLOAD_LENGTH = 5;
const int MTS310_ACCEL_0_HW_CH = 8;
const int MTS310_ACCEL_1_HW_CH = 9;
const unsigned short ACCEL_SENSOR_PID = 10;
const char SOS_OK = 17;
const int EINVAL = 18;



typedef unsigned int func_cb_ptr;



typedef struct Message {
	unsigned short  did;                           //!< module destination id
	unsigned short  sid;                           //!< module source id
	unsigned short daddr;                          //!< node destination address
	unsigned short saddr;                          //!< node source address
	unsigned char  type;                           //!< module specific message type
	unsigned char  len;                            //!< payload length 
	unsigned char  *data;                          //!< actual payload
	unsigned short flag;                           //!< flag to indicate the status of message, see below
	unsigned char payload[4];                      //!< statically allocated payload
	struct Message *next;                          //!< link list for the Message
} Message;



typedef struct accel_sensor_state {
    unsigned char accel_0_state;
    unsigned char accel_1_state;
    unsigned char options;
    unsigned char state;
} accel_sensor_state_t;



char accel_data_ready_cb(func_cb_ptr cb, unsigned char port, unsigned short value, unsigned char flags);
static char accel_control(func_cb_ptr cb, unsigned char cmd, void *data);
static char accel_msg_handler(void *state, Message *msg);



// Dummy functions implmented in other parts of the system
void ker_sensor_data_ready(int sid, unsigned short value, unsigned char flags);
void ker_adc_proc_bindPort(unsigned int w, unsigned int x, unsigned int y,  unsigned int z);
void ker_sensor_register(unsigned short pid, unsigned int sid, unsigned int fid, void *state);
void ker_adc_proc_unbindPort(unsigned short pid, int sid);
void ker_sensor_deregister(unsigned short pid, int sid);
char ker_adc_proc_getData(int sid, int id);
void sys_free(void * buf);


// data ready callback registered with adc driver
char accel_data_ready_cb(func_cb_ptr cb, unsigned char port, unsigned short value, unsigned char flags) {

    if (port == MTS310_ACCEL_0_SID) {
        ker_sensor_data_ready(MTS310_ACCEL_0_SID, value, flags);
    } else {
        ker_sensor_data_ready(MTS310_ACCEL_1_SID, value, flags);
    }
    return SOS_OK;
}



static inline void accel_on() {
    // SET_ACCEL_EN();
    // SET_ACCEL_EN_DD_OUT();
}
static inline void accel_off() {
    // SET_ACCEL_EN_DD_IN();
    // CLR_ACCEL_EN();
}



// function registered with kernel sensor component
static char accel_control(func_cb_ptr cb, unsigned char cmd, void* data) {\

    unsigned char ctx = *(unsigned char*)data;

    switch (cmd) {
        case SENSOR_GET_DATA_CMD:
            // get ready to read accel sensor
            if ((ctx & 0xC0) == ACCEL_0_SENSOR_ID) {
                return ker_adc_proc_getData(MTS310_ACCEL_0_SID, ACCEL_0_SENSOR_ID);
            } else {
                return ker_adc_proc_getData(MTS310_ACCEL_1_SID, ACCEL_1_SENSOR_ID);
            }
            break;

        case SENSOR_ENABLE_CMD:
            accel_on();
            break;

        case SENSOR_DISABLE_CMD:
            accel_off();
            break;

        case SENSOR_CONFIG_CMD:
            // no configuation
            if (data != NULL) {
                sys_free(data);
            }
            break;

        default:
            return -EINVAL;
    }
    return SOS_OK;
}


// message handler called by kernel
char accel_msg_handler(void *state, Message *msg)
{

    accel_sensor_state_t *s = (accel_sensor_state_t*)state;

    switch (msg->type) {

        case MSG_INIT:
            
            // bind adc channel and register callback pointer
            ker_adc_proc_bindPort(MTS310_ACCEL_0_SID, MTS310_ACCEL_0_HW_CH, ACCEL_SENSOR_PID,  SENSOR_DATA_READY_FID);
            ker_adc_proc_bindPort(MTS310_ACCEL_1_SID, MTS310_ACCEL_1_HW_CH, ACCEL_SENSOR_PID,  SENSOR_DATA_READY_FID);
           
            // register with kernel sensor interface
            s->accel_0_state = ACCEL_0_SENSOR_ID;
            ker_sensor_register(ACCEL_SENSOR_PID, MTS310_ACCEL_0_SID, SENSOR_CONTROL_FID, (void*)(&s->accel_0_state));
            s->accel_1_state = ACCEL_1_SENSOR_ID;
            ker_sensor_register(ACCEL_SENSOR_PID, MTS310_ACCEL_1_SID, SENSOR_CONTROL_FID, (void*)(&s->accel_1_state));
            break;

        case MSG_FINAL:
            // shutdown sensor
            accel_off();
            //  unregister ADC port
            ker_adc_proc_unbindPort(ACCEL_SENSOR_PID, MTS310_ACCEL_0_SID);
            ker_adc_proc_unbindPort(ACCEL_SENSOR_PID, MTS310_ACCEL_1_SID);
            // unregister sensor
            ker_sensor_deregister(ACCEL_SENSOR_PID, MTS310_ACCEL_0_SID);
            ker_sensor_deregister(ACCEL_SENSOR_PID, MTS310_ACCEL_1_SID);
            break;

        default:
            return -EINVAL;
            break;
    }
    return SOS_OK;
}


