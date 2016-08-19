#ifndef HS_SPI_H
#define HS_SPI_H

#include <linux/types.h>
#include <stddef.h>
#include <stdio.h>

unsigned char spi_mode_0();
unsigned char spi_mode_1();
unsigned char spi_mode_2();
unsigned char spi_mode_3();

int spi_transfer_message_1(int fd, void* buff, __u32 buff_len, __u32 speed, __u8 bits);

int spi_set_rd_mode(int fd, __u8 mode);
int spi_set_wr_mode(int fd, __u8 mode);

int spi_set_rd_lbs_first(int fd, __u8 lbs);
int spi_set_wr_lbs_first(int fd, __u8 lbs);

int spi_set_rd_bits_per_word(int fd, __u8 bits);
int spi_set_wr_bits_per_word(int fd, __u8 bits);

int spi_set_rd_max_speed_hz(int fd, __u32 speed);
int spi_set_wr_max_speed_hz(int fd, __u32 speed);


#endif
