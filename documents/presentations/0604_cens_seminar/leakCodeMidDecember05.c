sos_module_op_t *mod_op;
if (msg->saddr == ker_id() || s->pend) {
        return SOS_OK;
}
mod_op = (sos_module_op_t*) ker_msg_take_data(msg);
if(mod_op == NULL) return -ENOMEM;
switch(mod_op->op){
    case MODULE_OP_INSMOD:
            return module_op_insmod(s,msg,mod_op); 
    case MODULE_OP_RMMOD:
                return module_op_rmmod(s,msg,mod_op);
}
return SOS_OK;
