#!/bin/sh

S=500
D=h-$S
T="x"$S

for f in jpeg
do
    for i in *.$f
    do
        n=$(basename $i .$f)
        echo "resize: $n"
        if test -f $D/$n.jpeg
        then echo "$D/$n.jpeg exists"
        else convert -resize $T -colorspace rgb $i $D/$n.jpeg
        fi
    done
done
