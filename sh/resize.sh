#!/bin/sh

d=$1

for s in 150 500
do
    o=h-$s
    mkdir -p $d/$o
    t="x"$s
    for f in jpeg
    do
        for i in $d/*.$f
        do
            n=$(basename $i .$f)
            echo "resize: $n"
            if test -f $d/$o/$n.jpeg
            then echo "$d/$o/$n.jpeg exists"
            else convert -resize $t -colorspace rgb $i $d/$o/$n.jpeg
            fi
        done
    done
done
