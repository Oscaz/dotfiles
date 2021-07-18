#!/bin/bash
card="$(~/.xmonad/get_headset_card.sh 2>&1)"
if [ "$card" = "No PulseAudio daemon running, or not running as session daemon." ]; then
  echo "0"
  exit 0
fi
amixer -c "$(~/.xmonad/get_headset_card.sh)" get Headset,1 | awk -F'[][]' '/%/' | perl -pe 's/^.*(?:\[(\d{1,3})%\]).*$/\1/' | head -n 1
