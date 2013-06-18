#!/bin/sh

for i in *.jpg ; do mv $i $(basename $i .jpg).jpeg ; done
