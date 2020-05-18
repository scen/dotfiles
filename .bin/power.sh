#!/bin/zsh

confirm() {
    [[ $(echo -e "no,yes" | rofi -sep , -dmenu -i -p "confirm" -l 2 -width -20) == "yes" ]]
}

sleep() {
    confirm && ./lock.sh suspend
}

restart() {
    confirm && systemctl reboot
}

shutdown () {
    confirm && systemctl poweroff
}

opts="sleep,restart,shutdown"
case $(echo -e $opts | rofi -sep , -dmenu -i -p "pwr" -l 3 -width -20) in
    "sleep")
        sleep;;
    "restart")
        restart;;
    "shutdown")
        shutdown;;
esac
