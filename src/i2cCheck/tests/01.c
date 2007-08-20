extern int sys_i2c_reserve_bus();
extern int sys_i2c_release_bus();
extern int sys_i2c_send_data();
extern int sys_i2c_read_data();

int main() {
    sys_i2c_release_bus();
    sys_i2c_reserve_bus();
    sys_i2c_reserve_bus();

    return 0;
}
