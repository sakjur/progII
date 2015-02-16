.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = cmplx brot ppm

all: compile 

compile: ${MODS:%=%.beam}

ppm:
	wget http://people.kth.se/~johanmon/courses/id1019/seminars/mandel/ppm.erl

clean:
	rm -rf *.beam erl_crash.dump

