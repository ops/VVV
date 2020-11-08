#Makefile for GNU Make by Marko Mäkelä
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

MODULES = basic.i timings.i global.i

TARGETS1 = 1-intro 3-copper 5-plasma
TARGETS2 = init 2-scroll 4-vector 6-rotator 7-end

DIRS = pal ntsc pal.u ntsc.u

ALLTARGETS = $(TARGETS1:%=pal.u/%) $(TARGETS1:%=ntsc.u/%) \
             $(TARGETS1:%=pal/%) $(TARGETS1:%=ntsc/%) \
             $(TARGETS2:%=pal/%) $(TARGETS2:%=ntsc/%)

all: $(DIRS) $(ALLTARGETS)

$(DIRS):
	mkdir $@

pal.u/% : %.s $(MODULES)
	dasm $< -o$@ -DSTANDALONE=1 -MSYSTEM=PAL
ntsc.u/% : %.s $(MODULES)
	dasm $< -o$@ -DSTANDALONE=1 -MSYSTEM=NTSC

pal/%: %.s $(MODULES)
	dasm $< -o$@ -DSTANDALONE=0 -MSYSTEM=PAL
ntsc/%: %.s $(MODULES)
	dasm $< -o$@ -DSTANDALONE=0 -MSYSTEM=NTSC

pal/2-scroll: 2-scroll.bin

ntsc/2-scroll: 2-scroll.bin

pal/7-end: font.big

ntsc/7-end: font.big

2-scroll.bin: 2-scroll.ppm ppm2bin
	./ppm2bin $< $@

font.big: font.multi convfont
	./convfont $< $@

ppm2bin: ppm2bin.c

convfont: convfont.c

clean:
	rm -f ppm2bin convfont listmultifont 2-scroll.bin font.big
	rm -f *.lst *.prg a.out

reallyclean: clean
	rm -fr $(DIRS)
