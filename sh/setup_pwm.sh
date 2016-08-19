#!/bin/sh

if [ "$#" -ne 3 ]
then
    echo "Usage: setup_pwm.sh [Chip Number] [Channel Number] [GPIO]"
    echo "ex) setup_pwm.sh 0 0 182"
    exit 1
fi


sh -c "echo mode1 > /sys/kernel/debug/gpio_debug/gpio$3/current_pinmux"
sh -c "echo $2    > /sys/class/pwm/pwmchip$1/export"
sh -c "chmod 666 /sys/class/pwm/pwmchip$1/pwm$2/enable"
sh -c "chmod 666 /sys/class/pwm/pwmchip$1/pwm$2/period"
sh -c "chmod 666 /sys/class/pwm/pwmchip$1/pwm$2/duty_cycle"
sh -c "echo 1 > /sys/class/pwm/pwmchip$1/pwm$2/enable"



