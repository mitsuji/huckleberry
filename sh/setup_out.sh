#!/bin/sh

if [ "$#" -ne 1 ]
then
    echo "Usage: setup_out.sh [GPIO]"
    echo "ex) setup_out.sh 78"
    exit 1
fi

sh -c "echo mode0 > /sys/kernel/debug/gpio_debug/gpio$1/current_pinmux"
sh -c "echo $1    > /sys/class/gpio/export"
sh -c "echo out   > /sys/class/gpio/gpio$1/direction"
sh -c "chmod 666 /sys/class/gpio/gpio$1/value"

