#!/bin/zsh

scrot /tmp/lock.jpg
convert /tmp/lock.jpg -scale 10% -scale 1000% /tmp/lock.jpg
i3lock -i /tmp/lock.jpg -u -c 00000000 -e
rm /tmp/lock.jpg

if [[ "$1" == "suspend" ]]
then
    systemctl suspend
else
    sleep 60
    pgrep i3lock && xset dpms force off
fi
