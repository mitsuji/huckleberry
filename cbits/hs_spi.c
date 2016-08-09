#include "hs_spi.h"

#include <sys/ioctl.h>
#include <linux/types.h>
#include <linux/spi/spidev.h>


unsigned char spi_mode_0() {
  return SPI_MODE_0;
}
unsigned char spi_mode_1() {
  return SPI_MODE_1;
}
unsigned char spi_mode_2() {
  return SPI_MODE_2;
}
unsigned char spi_mode_3() {
  return SPI_MODE_3;
}


int spi_transfer_message_1(int fd, void* buff, __u32 buff_len, __u32 speed, __u8 bits) {

  struct spi_ioc_transfer spi;
  spi.tx_buf        = (__u64) buff;
  spi.rx_buf        = (__u64) buff;
  spi.len           = buff_len;
  spi.speed_hz      = speed;
  spi.bits_per_word = bits;
  spi.delay_usecs   = 0;
  spi.cs_change     = 0;

  return ioctl(fd,SPI_IOC_MESSAGE(1),&spi);
}

int spi_set_rd_mode(int fd, __u8 mode) {
  return ioctl(fd,SPI_IOC_RD_MODE,&mode);
}
int spi_set_wr_mode(int fd, __u8 mode) {
  return ioctl(fd,SPI_IOC_WR_MODE,&mode);
}

int spi_set_rd_lbs_first(int fd, __u8 lbs) {
  return ioctl(fd,SPI_IOC_RD_LSB_FIRST,&lbs);
}
int spi_set_wr_lbs_first(int fd, __u8 lbs) {
  return ioctl(fd,SPI_IOC_WR_LSB_FIRST,&lbs);
}

int spi_set_rd_bits_per_word(int fd, __u8 bits) {
  return ioctl(fd,SPI_IOC_RD_BITS_PER_WORD,&bits);
}
int spi_set_wr_bits_per_word(int fd, __u8 bits) {
  return ioctl(fd,SPI_IOC_WR_BITS_PER_WORD,&bits);
}

int spi_set_rd_max_speed_hz(int fd, __u32 speed) {
  return ioctl(fd,SPI_IOC_RD_MAX_SPEED_HZ,&speed);
}
int spi_set_wr_max_speed_hz(int fd, __u32 speed) {
  return ioctl(fd,SPI_IOC_WR_MAX_SPEED_HZ,&speed);
}


