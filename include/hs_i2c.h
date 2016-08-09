#ifndef HS_I2C_H
#define HS_I2C_H

#include <linux/types.h>

int i2c_set_slave(int fd, __u8 slave);

#endif
