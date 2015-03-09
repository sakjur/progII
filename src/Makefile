.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = reverse sort test

all: compile

compile: ${MODS:%=%.beam} subdirs

subdirs:
	cd metaint; $(MAKE)
	cd philosophers; $(MAKE)
	cd Huffman; $(MAKE)
	cd mandelbrot; $(MAKE)
	cd webserver; $(MAKE)

clean:
	rm -rf *.beam erl_crash.dump
	cd metaint; $(MAKE) clean
	cd philosophers; $(MAKE) clean
	cd Huffman; $(MAKE) clean
	cd mandelbrot; $(MAKE) clean
	cd webserver; $(MAKE) clean

