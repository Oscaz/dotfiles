pacmd list-sources | sed '1,/alsa_input\.usb-Cosair_Corsair_VOID_ELITE_USB_Gaming_Headset_00000000-00\.analog-stereo/d' | grep 'alsa\.card =' | head -1 | cut -d '"' -f 2
