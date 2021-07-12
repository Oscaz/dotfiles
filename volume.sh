#!/bin/bash
amixer -c "$(~/.xmonad/get_headset_card.sh)" get Headset,1 | awk -F'[][]' '/%/' | perl -pe 's/^.*(?:\[(\d{1,3})%\]).*$/\1/' | head -n 1
