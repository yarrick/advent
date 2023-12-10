# Find the binary name for the source file most recently changed.
latest := $(shell ls -t *.hs | head -n 1 | cut -d. -f1)

# Build and run latest with test case files from variable F
# Example: make F="ex* input"
#
# Input file is fetched again when latest file changed.
all: ${latest}
	@if [ "${latest}" != "$$(cat .latest.txt)" ]; then \
		rm input; \
		$(MAKE) input; \
	fi
	@echo ${latest} > .latest.txt
ifneq ($(F),)
	@for i in `ls $F`; do echo "Running $$i"; time ./${latest} < $$i; done
endif

clean:
	rm -f *.hi *.o [0-2][0-9]

input:
	aoc -Io -d ${latest} download

%: %.hs
	ghc --make -O2 $<
