#!/bin/sh

setxkbmap -option caps:escape
/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
