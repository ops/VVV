#!/bin/bash

make
cl65 -t vic20 -C boot.cfg -o boot boot.s gijoe-loader.lib

c1541 -format "vvv 2020,os" d64 vvv-pal.d64
c1541 vvv-pal.d64 -write boot
c1541 vvv-pal.d64 -write pal/init 00
c1541 vvv-pal.d64 -write pal/1-intro 01
c1541 vvv-pal.d64 -write pal/2-scroll 02
c1541 vvv-pal.d64 -write pal/3-copper 03
c1541 vvv-pal.d64 -write pal/4-vector 04
c1541 vvv-pal.d64 -write pal/5-plasma 05
c1541 vvv-pal.d64 -write pal/6-rotator 06
c1541 vvv-pal.d64 -write pal/7-end 07

c1541 -format "vvv 2020,os" d64 vvv-ntsc.d64
c1541 vvv-ntsc.d64 -write boot
c1541 vvv-ntsc.d64 -write ntsc/init 00
c1541 vvv-ntsc.d64 -write ntsc/1-intro 01
c1541 vvv-ntsc.d64 -write ntsc/2-scroll 02
c1541 vvv-ntsc.d64 -write ntsc/3-copper 03
c1541 vvv-ntsc.d64 -write ntsc/4-vector 04
c1541 vvv-ntsc.d64 -write ntsc/5-plasma 05
c1541 vvv-ntsc.d64 -write ntsc/6-rotator 06
c1541 vvv-ntsc.d64 -write ntsc/7-end 07

rm boot
