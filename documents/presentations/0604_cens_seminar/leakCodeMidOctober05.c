mod_op = (sos_module_op_t*) ker_msg_take_data(msg);
if(mod_op == NULL) return -ENOMEM;
if(mod_op->op == MODULE_OP_INSMOD) {
    existing_module = ker_get_module(mod_op->mod_id);
    if(existing_module != NULL) {
        uint8_t ver = sos_read_header_byte(
                existing_module->header,
                offsetof(mod_header_t, version));
        if (ver < mod_op->version) {
            ker_unload_module(existing_module->pid, 
                    sos_read_header_byte(
                        existing_module->header,
                        offsetof(mod_header_t, version)));
        } else {
            return SOS_OK;
        }
    }
    ret = fetcher_request(KER_DFT_LOADER_PID,
            mod_op->mod_id,
            mod_op->version,
            entohs(mod_op->size),
            msg->saddr);
    s->pend = mod_op;
    ker_led(LED_RED_TOGGLE);
    return SOS_OK;
}
return SOS_OK;

