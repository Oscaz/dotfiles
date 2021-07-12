#!/bin/bash
state="$(cat /opt/pia_status/state)"

if echo "$state" | grep -q 'Connected'; then
  vpn_ip="$(cat /opt/pia_status/ip)"
  echo "<fc=#34e067><icon=/home/oscar/.xmonad/lock.xpm/> $vpn_ip</fc>"
elif echo "$state" | grep -q 'Disconnected'; then
  echo "<fc=#f0243f><icon=/home/oscar/.xmonad/lock-off.xpm/> Disconnected</fc>"
else
  echo "<fc=#fa8c0f><icon=/home/oscar/.xmonad/lock-broken.xpm/> $state</fc>"
fi
