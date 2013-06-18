#!/bin/sh

for i in *.jpeg
do
    darcs add $i
    darcs rec -a -m "+$i"
done
