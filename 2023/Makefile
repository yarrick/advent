# Find the binary name for the most recently changed source file
days := $(shell ls -t [0-9][0-9].hs | cut -d. -f1)
latest := $(firstword $(days))
year := $(shell basename $(CURDIR))

# Build latest
build: ${latest}
	@

# Build all
all: ${days}
	@

# Build and run latest with example inputs
test: ${latest}
	@../.run.sh ./${latest} example/${latest}*

# Build and run latest with private input
run: ${latest} input/${latest}
	@../.run.sh ./${latest} input/${latest}

# Remove built files and logs
clean:
	rm -f *.hi *.o [0-2][0-9] .rts_stats*log

# Fetch input for a day
# Depends on https://github.com/scarvalhojr/aoc-cli
# and an existing session cookie.
input/%:
	@mkdir -p input
	aoc -Io -y ${year} -d $(shell basename $@) -i $@ download

%: %.hs
	ghc --make -O2 -rtsopts $<
