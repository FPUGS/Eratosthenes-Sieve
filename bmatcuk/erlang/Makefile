.PHONY: all
all: sieve.beam

%.beam: %.erl
	erlc $<

.PHONY: run
run: all
	erl -s sieve test -s init stop

