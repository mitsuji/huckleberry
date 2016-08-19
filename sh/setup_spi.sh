#!/bin/sh

if [ "$#" -ne 6 ]
then
    echo "Usage: setup_spi.sh [Bus Number] [ChipSelect Number] [CLK] [MOSI] [MISO] [SS]"
    echo "ex) setup_spi.sh 5 1 109 114 115 111"
    exit 1
fi

sh -c "echo mode1 > /sys/kernel/debug/gpio_debug/gpio$3/current_pinmux"
sh -c "echo mode1 > /sys/kernel/debug/gpio_debug/gpio$4/current_pinmux"
sh -c "echo mode1 > /sys/kernel/debug/gpio_debug/gpio$5/current_pinmux"
sh -c "echo mode1 > /sys/kernel/debug/gpio_debug/gpio$6/current_pinmux"
sh -c "chmod 666 /dev/spidev$1.$2"

