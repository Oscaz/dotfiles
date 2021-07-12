gpu="$(~/.xmonad/gpu_util.sh)"
color="#ffffff"

if [ "$gpu" -ge "5" ]; then
  color="#00FF00"
  if [ "$gpu" -ge "50" ]; then
    color="#FF0000"
  fi
fi

echo "gpu: (<fc=$color>$gpu</fc>%)"
