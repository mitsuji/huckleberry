#!/bin/sh

if [ "$#" -ne 3 ]
then
    echo "Usage: setup_i2c.sh [Adapter Number] [SCL] [SDA]"
    echo "ex) setup_i2c.sh 6 27 28"
    exit 1
fi

sh -c "echo mode1 > /sys/kernel/debug/gpio_debug/gpio$2/current_pinmux"
sh -c "echo mode1 > /sys/kernel/debug/gpio_debug/gpio$3/current_pinmux"
sh -c "chmod 666 /dev/i2c-$1"


