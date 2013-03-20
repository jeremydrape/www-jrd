#!/bin/sh

for s in 150 500
do
    d=h-$s
    mkdir -p $d
    t="x"$s
    for f in jpeg
    do
        for i in *.$f
        do
            n=$(basename $i .$f)
            echo "resize: $n"
            if test -f $d/$n.jpeg
            then echo "$d/$n.jpeg exists"
            else convert -resize $t -colorspace rgb $i $d/$n.jpeg
            fi
        done
    done
done
