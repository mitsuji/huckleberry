#include "hs_spi.h"

#include <sys/ioctl.h>
#include <linux/spi/spidev.h>
#include <string.h>


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



int spi_transfer_tx_rx_1(int fd, void *buff, __u32 buff_len, __u8 bits, __u32 speed, __u16 delay, __u8 cs_change) {

  struct spi_ioc_transfer tran;
  memset(&tran,0,sizeof(struct spi_ioc_transfer));
  
  tran.tx_buf        = (__u64) buff;
  tran.rx_buf        = (__u64) buff;
  tran.len           = buff_len;
  tran.bits_per_word = bits;
  tran.speed_hz      = speed;
  tran.delay_usecs   = delay;
  tran.cs_change     = cs_change;

  return ioctl(fd,SPI_IOC_MESSAGE(1),&tran);
  
}
int spi_transfer_tx_rx_2(int fd,
			 void *tx_buff, __u32 tx_buff_len,
			 void *rx_buff, __u32 rx_buff_len,
			 __u8 bits, __u32 speed, __u16 delay, __u8 cs_change) {

  struct spi_ioc_transfer tran[2];
  memset(tran,0,sizeof(tran));
  
  tran[0].tx_buf        = (__u64) tx_buff;
  tran[0].len           = tx_buff_len;
  tran[0].bits_per_word = bits;
  tran[0].speed_hz      = speed;
  tran[0].delay_usecs   = delay;
  tran[0].cs_change     = cs_change;

  tran[1].rx_buf        = (__u64) rx_buff;
  tran[1].len           = rx_buff_len;
  tran[1].bits_per_word = bits;
  tran[1].speed_hz      = speed;
  tran[1].delay_usecs   = delay;
  tran[1].cs_change     = cs_change;

  return ioctl(fd,SPI_IOC_MESSAGE(2),tran);
  
}
int spi_transfer_tx_1(int fd, void *buff, __u32 buff_len, __u8 bits, __u32 speed, __u16 delay, __u8 cs_change) {
  
  struct spi_ioc_transfer tran;
  memset(&tran,0,sizeof(struct spi_ioc_transfer));
  
  tran.tx_buf        = (__u64) buff;
  tran.len           = buff_len;
  tran.bits_per_word = bits;
  tran.speed_hz      = speed;
  tran.delay_usecs   = delay;
  tran.cs_change     = cs_change;

  return ioctl(fd,SPI_IOC_MESSAGE(1),&tran);

}
int spi_transfer_rx_1(int fd, void *buff, __u32 buff_len, __u8 bits, __u32 speed, __u16 delay, __u8 cs_change) {
  
  struct spi_ioc_transfer tran;
  memset(&tran,0,sizeof(struct spi_ioc_transfer));
  
  tran.rx_buf        = (__u64) buff;
  tran.len           = buff_len;
  tran.bits_per_word = bits;
  tran.speed_hz      = speed;
  tran.delay_usecs   = delay;
  tran.cs_change     = cs_change;

  return ioctl(fd,SPI_IOC_MESSAGE(1),&tran);

}


int spi_get_mode(int fd, __u8 *pmode) {
  return ioctl(fd,SPI_IOC_RD_MODE,pmode);
}
int spi_set_mode(int fd, __u8 mode) {
  return ioctl(fd,SPI_IOC_WR_MODE,&mode);
}

int spi_get_lsb_first(int fd, __u8 *plsb) {
  return ioctl(fd,SPI_IOC_RD_LSB_FIRST,plsb);
}
int spi_set_lsb_first(int fd, __u8 lsb) {
  return ioctl(fd,SPI_IOC_WR_LSB_FIRST,&lsb);
}

int spi_get_bits_per_word(int fd, __u8 *pbits) {
  return ioctl(fd,SPI_IOC_RD_BITS_PER_WORD,pbits);
}
int spi_set_bits_per_word(int fd, __u8 bits) {
  return ioctl(fd,SPI_IOC_WR_BITS_PER_WORD,&bits);
}

int spi_get_max_speed_hz(int fd, __u32 *pspeed) {
  return ioctl(fd,SPI_IOC_RD_MAX_SPEED_HZ,pspeed);
}
int spi_set_max_speed_hz(int fd, __u32 speed) {
  return ioctl(fd,SPI_IOC_WR_MAX_SPEED_HZ,&speed);
}


