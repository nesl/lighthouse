sys_i2c_reserve_bus.pre {
    free();
}

sys_i2c_reserve_bus.post {
    reserved();
}


sys_i2c_release_bus.pre {
    reserved();
}

sys_i2c_release_bus.post {
    free();
}


sys_i2c_send_data.pre {
    reserved();
}

sys_i2c_send_data.post {
    reserved();
}


sys_i2c_read_data.pre {
    reserved();
}

sys_i2c_read_data.post {
    reserved();
}


