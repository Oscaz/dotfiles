#!/bin/bash
BARS=10

vol=$(~/.xmonad/volume.sh)
status=$(~/.xmonad/volume_on_off.sh)

greenBars=$(("$vol" * "$BARS" / 100))
redBars=$(("$BARS" - "$greenBars"))

if test "$vol" -gt 0 -a "$vol" -lt $((100 / "$BARS")); then
  greenBars=1
  redBars=$(("$BARS"-1))
fi

printf '<fc=#878787>['

printf '<fc=#0af751>'
for ((i=0; i<"$greenBars"; i=$(("$i"+1))))
do
    printf '#'
done
printf '</fc>'

printf '<fc=#fa1414>'
for ((i=0; i<"$redBars"; i=$(("$i"+1))))
do
    printf '#'
done
printf '</fc>'

printf ']</fc>'

printf "<fc=#bababa> Vol: $vol%% </fc>"

if echo "$status" | grep -q 'on'; then
  printf '<fc=#0af751>'
elif echo "$status" | grep -q 'off'; then
  printf '<fc=#fa1414>'
fi

echo "[$status] </fc>"
