#!/bin/bash

icons_dir=/usr/share/icons/mate/scalable/status
vol_mute=audio-volume-muted-symbolic.svg
vol_high=audio-volume-high-symbolic.svg
vol_med=audio-volume-medium-symbolic.svg
vol_low=audio-volume-low-symbolic.svg
icon=$vol_high

if [ $2 == "mute" ]; then
    pactl set-sink-mute $1 toggle
    notify-send " " -i $icons_dir/$vol_mute
    exit 0
fi


pactl set-sink-volume $1 $2 $3

VOL=`pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,'`

echo $VOL

if [ $VOL -gt 66 ]; then
    icon=$vol_high
elif [ $VOL -gt 33 ]; then
    icon=$vol_med
else
    icon=$vol_low
fi



notify-send " " -i $icons_dir/$icon -h int:value:$VOL -h string:synchronous:volume -t 1000
