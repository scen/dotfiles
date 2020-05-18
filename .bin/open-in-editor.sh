#!/bin/zsh

editor="code"

file=$(eval $FZF_DEFAULT_COMMAND | rofi -dmenu -i -p "open file in editor")
[[ "$file" != "" ]] && $editor $file