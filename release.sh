#!/bin/bash
NAME="topl-$(date +%Y%m)"
rm -rf $NAME
mkdir $NAME
make native
cp toplc $NAME
cp -r src $NAME
cp -r examples $NAME
cp -r ~/custom/share/camomile $NAME
tar caf $NAME.tar.bz2 $NAME/*
rm -rf $NAME
