#!/bin/bash

card=$(~/.xmonad/get_headset_card.sh)

if echo "$1" | grep -q 'up'; then
  amixer -c "$card" set Headset,1 5%+ unmute >/dev/null 2>&1
elif echo "$1" | grep -q 'down'; then
  amixer -c "$card" set Headset,1 5%- unmute >/dev/null 2>&1
elif echo "$1" | grep -q 'toggle'; then
  amixer -c "$card" set Headset,1 toggle >/dev/null 2>&1
fi
