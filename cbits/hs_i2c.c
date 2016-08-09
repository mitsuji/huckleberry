#include "hs_i2c.h"

#include <sys/ioctl.h>
#include <linux/types.h>
#include <linux/i2c-dev.h>


int i2c_set_slave(int fd, __u8 slave) {
  return ioctl(fd,I2C_SLAVE,slave);
}
