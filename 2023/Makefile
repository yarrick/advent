# Find the binary name for the source file most recently changed.
latest := $(shell ls -t *.hs | head -n 1 | cut -d. -f1)
year := $(shell basename $(CURDIR))

# Build and run latest with test case files from variable F
# Example: make F="ex* input"
#
# Input file is fetched again when latest file changed.
all: ${latest}
	@if [ -f .latest.txt ] && [ "${latest}" != "$$(cat .latest.txt)" ]; then \
		rm input; \
		$(MAKE) input; \
	fi
	@echo ${latest} > .latest.txt
ifneq ($(F),)
	@for i in `ls $F`; do echo "Running $$i"; time ./${latest} +RTS -M10g -s.rts_stats_$$i.log < $$i; done
endif

clean:
	rm -f *.hi *.o [0-2][0-9] .rts_stats*log

# Depends on https://github.com/scarvalhojr/aoc-cli
# and an existing session cookie.
input:
	aoc -Io -y ${year} -d ${latest} download

%: %.hs
	ghc --make -O2 -rtsopts $<
