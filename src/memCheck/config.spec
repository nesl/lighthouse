malloc.pre {}

malloc.post {
    $return.full();
}

free.pre {
    $1.full();
}

free.post {
    $1.empty();
}

ker_malloc.pre {}

ker_malloc.post {
    $return.full()
}

ker_free.pre {
    $1.full();
}

ker_free.post {
    $1.empty();
}

blk_mem_alloc.pre {}

blk_mem_alloc.post {
    $return.full();
}

blk_mem_free.pre {
    $1.full();
}

blk_mem_free.post {
    $1.empty();
}

post_long.pre {
    $5.full();
}

post_long.post {
    $5.either();
}

post_link.pre {
    $5.full();
}

post_link.post {
    $5.either();
}

post_auto.pre {
    $5.full();
}

post_auto.post {
    $5.either();
}

post_net.pre {
    $5.full();
}

post_net.post {
    $5.either();
}

post_uart.pre {
    $5.full();
}

post_uart.post {
    $5.either();
}

post_i2c.pre {
    $5.full();
}

post_i2c.post {
    $5.either();
}

post_spi.pre {
    $5.full();
}

post_spi.post {
    $5.either();
}

ker_msg_take_data.pre {}

ker_msg_take_data.post {
    $return.full();
}

msg_duplicate.pre {}

msg_duplicate.post {
    $return.full();
}

msg_create.pre {}

msg_create.post {
    $return.full();
}

handle_incoming_msg.pre {
    $1.full();
}

handle_incoming_msg.post {
    $1.empty();
}

sched_msg_alloc.pre {
    $1.full();
}

sched_msg_alloc.post {
    $1.empty();
}

mq_enqueue.pre {
    $2.full();
}

mq_enqueue.post {
    $2.empty();
}

ker_cam_add.pre {
    $2.full();
}

ker_cam_add.post {
    $2.empty();
}

msg_dispatch.pre {
    $1.full();
}

msg_dispatch.post {
    $1.empty;
}

pop_new_mod_op.pre {}

pop_new_mod_op.post {
    $return.full();
}


ker_change_own.pre {
    $1.empty();
}

ker_change_own.post {
    $1.full();
}

sys_msg_take_data.pre {}

sys_msg_take_data.post {
    $return.full();
}

fetcher_restart.pre {
    $1.full();
}

fetcher_restart.post {
    $1.empty();
}

sos_msg_dispatch.pre {
    $1.full();
}

sos_msg_dispatch.post {
    $1.empty();
}

ker_sensor_control.pre {
    $3.full();
}

ker_sensor_control.post {
    $3.empty();
}

sys_malloc.pre {}

sys_malloc.post {
    $return.full();
}

sys_free.pre {
    $1.full();
}

sys_free.post {
    $1.empty();
}

