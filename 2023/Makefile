# Find the binary name for the most recently changed source file
days := $(shell ls -t [0-9][0-9].hs | cut -d. -f1)
latest := $(firstword $(days))
year := $(shell basename $(CURDIR))

# Default to latest changed file.
# Can be overridden: `make run day=20`
day := ${latest}

# Build binary for day
build: ${day}
	@

# Build all
all: ${days}
	@

# Build and run with example inputs
test: ${day}
	@../.run.sh ./${day} example/${day}*

# Build and run with private input
run: ${day} input/${day}
	@../.run.sh ./${day} input/${day}

# Build and run with private input, print only elapsed time
bench: ${day} input/${day}
	@../.run.sh ./${day} input/${day} 2>&1 | grep "^real" | awk '{ print $$2 }'

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
	@>&2 ghc --make -O2 -rtsopts ${ghcopts} $< -package bytestring -package deepseq
