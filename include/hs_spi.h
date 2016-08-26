#ifndef HS_SPI_H
#define HS_SPI_H

#include <linux/types.h>

unsigned char spi_mode_0();
unsigned char spi_mode_1();
unsigned char spi_mode_2();
unsigned char spi_mode_3();

int spi_transfer_tx_rx_1(int fd, void *buff, __u32 buff_len, __u8 bits, __u32 speed, __u16 delay, __u8 cs_change);
int spi_transfer_tx_rx_2(int fd,
			 void *tx_buff, __u32 tx_buff_len,
			 void *rx_buff, __u32 rx_buff_len,
			 __u8 bits, __u32 speed, __u16 delay, __u8 cs_change);
int spi_transfer_tx_1(int fd, void *buff, __u32 buff_len, __u8 bits, __u32 speed, __u16 delay, __u8 cs_change);
int spi_transfer_rx_1(int fd, void *buff, __u32 buff_len, __u8 bits, __u32 speed, __u16 delay, __u8 cs_change);

int spi_get_mode(int fd, __u8 *pmode);
int spi_set_mode(int fd, __u8 mode);

int spi_get_lsb_first(int fd, __u8 *plsb);
int spi_set_lsb_first(int fd, __u8 lsb);

int spi_get_bits_per_word(int fd, __u8 *pbits);
int spi_set_bits_per_word(int fd, __u8 bits);

int spi_get_max_speed_hz(int fd, __u32 *pspeed);
int spi_set_max_speed_hz(int fd, __u32 speed);


#endif
