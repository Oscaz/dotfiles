#! /bin/bash
field=()
field+=("aa")
field+=("bb")
field+=("cc")
echo Num items: ${#field[@]}
echo Data: ${field[@]}
