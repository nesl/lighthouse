enum {SENSOR_DATA_READY_FID,
    SENSOR_CONTROL_FID,
    SENSOR_GET_DATA_CMD,
    SENSOR_ENABLE_CMD,
    SENSOR_DISABLE_CMD,
    SENSOR_CONFIG_CMD,
    MSG_INIT,
    MSG_FINAL,
    ACCEL_SENSOR_PID,
    HW_TYPE,
    MCU_TYPE,
};


typedef unsigned int func_cb_ptr;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef char int8_t;
typedef short sos_pid_t;
typedef short sos_code_id_t;

const int NULL = 0;
const int ACCEL_0_SENSOR_ID = 1;
const int ACCEL_1_SENSOR_ID = 2;
const int MTS310_ACCEL_0_SID = 3;
const int MTS310_ACCEL_1_SID = 4;
const int SOS_MSG_PAYLOAD_LENGTH = 5;
const int MTS310_ACCEL_0_HW_CH = 8;
const int MTS310_ACCEL_1_HW_CH = 9;
const char SOS_OK = 17;
const int EINVAL = 18;


typedef struct func_cb {
  void *ptr;        //! function pointer                    
  uint8_t proto[4]; //! function prototype                  
  uint8_t pid;      //! function PID                                    
  uint8_t fid;      //! function ID                         
} func_cb_t;



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

typedef int8_t (*msg_handler_t)(void *state, Message *m);


typedef struct mod_header {
  uint16_t state_size;      //!< module state size
  sos_pid_t mod_id;        //!< module ID (used for messaging).  Set NULL_PID for system selected mod_id
  uint8_t num_timers;      //!< Number of timers to be reserved at module load time
  uint8_t num_sub_func;    //!< number of functions to be subscribed
  uint8_t num_prov_func;   //!< number of functions provided
  uint8_t num_dfunc;       //!< number of direct linked functions
  uint8_t version;         //!< version number, for users bookkeeping
  sos_code_id_t code_id;   //!< module image identifier
  uint8_t processor_type;  //!< processor type of this module
  uint8_t platform_type;   //!< platform type of this module
  //  uint8_t padding;
  msg_handler_t module_handler;
  func_cb_t funct[];
} mod_header_t;



typedef struct accel_sensor_state {
	uint8_t accel_0_state;
	uint8_t accel_1_state;
	uint8_t options;
	uint8_t state;
} accel_sensor_state_t;


//const short ehtons(short e) {return e;}


// Dummy functions implmented in other parts of the system
void ker_sensor_data_ready(int sid, unsigned short value, unsigned char flags);
void ker_adc_proc_bindPort(unsigned int w, unsigned int x, unsigned int y,  unsigned int z);
void ker_sensor_register(unsigned short pid, unsigned int sid, unsigned int fid, void *state);
void ker_adc_proc_unbindPort(unsigned short pid, int sid);
void ker_sensor_deregister(unsigned short pid, int sid);
char ker_adc_proc_getData(int sid, int id);
void sys_free(void * buf);

// function registered with kernel sensor component
static int8_t accel_control(func_cb_ptr cb, uint8_t cmd, void *data);
// data ready callback registered with adc driver
int8_t accel_data_ready_cb(func_cb_ptr cb, uint8_t port, uint16_t value, uint8_t flags);

static int8_t accel_msg_handler(void *state, Message *msg);


static const mod_header_t mod_header = {
  mod_id : ACCEL_SENSOR_PID,
  state_size : sizeof(accel_sensor_state_t),
  num_timers : 0,
  num_sub_func : 0,
  num_prov_func : 2,
	platform_type : HW_TYPE,
	processor_type : MCU_TYPE,
	//code_id : ehtons(ACCEL_SENSOR_PID),
	code_id : ACCEL_SENSOR_PID,
  module_handler : accel_msg_handler,
	funct : {
		{accel_control, "cCw2", ACCEL_SENSOR_PID, SENSOR_CONTROL_FID},
		{accel_data_ready_cb, "cCS3", ACCEL_SENSOR_PID, SENSOR_DATA_READY_FID},
	},
};


/**
 * adc call back
 * not a one to one mapping so not SOS_CALL
 */
int8_t accel_data_ready_cb(func_cb_ptr cb, uint8_t port, uint16_t value, uint8_t flags) {

	// post data ready message here
	if (port == MTS310_ACCEL_0_SID) {
		ker_sensor_data_ready(MTS310_ACCEL_0_SID, value, flags);
	} else {
		ker_sensor_data_ready(MTS310_ACCEL_1_SID, value, flags);
	}
	return SOS_OK;
}


static inline void accel_on() {
	//SET_ACCEL_EN();
	//SET_ACCEL_EN_DD_OUT();
}
static inline void accel_off() {
	//SET_ACCEL_EN_DD_IN();
	//CLR_ACCEL_EN();
}

static int8_t accel_control(func_cb_ptr cb, uint8_t cmd, void* data) {\

	uint8_t ctx = *(uint8_t*)data;
	
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


int8_t accel_msg_handler(void *state, Message *msg)
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

/*
#ifndef _MODULE_
mod_header_ptr accel_sensor_get_header() {
	return sos_get_header_address(mod_header);
}
#endif
*/
