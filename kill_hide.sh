#!/bin/bash

T0=$(date +%s)

while [ $(( `date +%s` - T0 )) -lt 10 ]; do
  [ -z "$(xdotool search --classname 'hide_aquarium' windowkill %@ 2>&1)" ] && break
  sleep 0.1
done

while [ $(( `date +%s` - T0 )) -lt 10 ]; do
  [ -z "$(xdotool search --classname 'hide_cbonsai' windowkill %@ 2>&1)" ] && break
  sleep 0.1
done

while [ $(( `date +%s` - T0 )) -lt 10 ]; do
  [ -z "$(xdotool search --classname 'hide_text' windowkill %@ 2>&1)" ] && break
  sleep 0.1
done

#xdotool search --classname 'hide_cmatrix' windowkill %@
#xdotool search --classname 'hide_text' windowkill %@
