#!/bin/bash
fname=$(echo $1 | sed 's/.png//g')
outname="$2"
scala imgcompress.jar encode "$fname.png" "$fname.dct" "$2"
scala imgcompress.jar decode "$fname.dct" "${fname}_${outname}.png"
