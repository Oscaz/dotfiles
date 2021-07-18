#!/bin/bash
card="$(~/.xmonad/get_headset_card.sh 2>&1)"
if [ "$card" = "No PulseAudio daemon running, or not running as session daemon." ]; then
  echo "off"
  exit 0
fi
amixer -c "$(~/.xmonad/get_headset_card.sh)" | sed "1,/'Headset',1/d" | grep 'Front Left:' | sed -E 's/.*\[(\w+)\]\s*$/\1/'
