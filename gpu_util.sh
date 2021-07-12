#!/bin/bash

gpu="$(gpustat --no-color | sed -n 2p | cut -d '|' -f 2 | tr -s ' ' | cut -d ' ' -f 3)"
len="$(echo $gpu | wc -c)"

if [ "$len" = "2" ]; then
  gpu="0$gpu"
fi

echo "$gpu"
