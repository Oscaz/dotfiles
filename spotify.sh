#!/bin/bash

running=$(pidof spotify)
ctl="$(playerctl status 2>&1)"
npf="No players found"

if [ "$ctl" = "$npf" ]; then
   echo "No Track"
fi

if [ -n "running" ]; then
    if [ "$(playerctl -p spotify status)" = "Playing" ]; then
        artist="$(playerctl -p spotify metadata artist 2>/dev/null)"
        if [ "$artist" = "" ]; then
            artist="Unknown"
        fi
        song="$(playerctl -p spotify metadata title 2>/dev/null | cut -c 1-60 2>/dev/null)"
        echo "$artist · $song"
	exit 0
    fi
fi

playerctl -l | while read player; do
#    playing+=("$player")
#    a="$(playerctl -p $player play-pause)"
    if [ "$(playerctl -p $player status)" = "Playing" ]; then
            artist="$(playerctl -p $player metadata artist 2>/dev/null)"
        if [ "$artist" = "" ]; then
            artist="Unknown"
        fi
        song="$(playerctl -p $player metadata title 2>/dev/null | cut -c 1-60 2>/dev/null)"
        echo "$artist · $song"
	exit 0
    fi
done

echo "No Track"
