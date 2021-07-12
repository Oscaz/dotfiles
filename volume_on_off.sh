#!/bin/bash
amixer -c "$(~/.xmonad/get_headset_card.sh)" | sed "1,/'Headset',1/d" | grep 'Front Left:' | sed -E 's/.*\[(\w+)\]\s*$/\1/'
