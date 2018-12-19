######################################################
COURSE=cs131f
ORG=ucsd-cse131-fa18
ASGN=06
COMPILER=fox
EXT=fox
HEAP=100000
######################################################

COMPILEREXEC=stack exec -- $(COMPILER)
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
else
ifeq ($(UNAME), CYGWIN_NT-10.0)
  FORMAT=win
  WINSTUFF=-target i686-pc-mingw32
endif

endif
endif

CLANG=clang $(WINSTUFF) -g -m32 -mstackrealign # -fsanitize=address

.PHONY: test bin build clean distclean turnin \
	$(ASMS) $(OBJS) $(RUNS) $(RESULTS)

SHELL=/bin/bash -o pipefail

test: clean
	stack test

build:
	stack build

tests/output/%.vresult: tests/output/%.run FORCE
	valgrind $< $(HEAP) > $@
tests/output/%.result: tests/output/%.run FORCE
	$< $(HEAP) > $@


tests/output/%.run: tests/output/%.o c-bits/main.c c-bits/gc.o c-bits/types.o FORCE
	$(CLANG) -o $@ c-bits/main.c c-bits/gc.c c-bits/types.c $<

tests/output/%.o: tests/output/%.s FORCE
	nasm -f $(FORMAT) -o $@ $<

tests/output/%.s: tests/input/%.$(EXT) FORCE
	$(COMPILEREXEC) $< > $@

c-bits/gc.o: c-bits/gc.c c-bits/gc.h
	$(CLANG) -c -o c-bits/gc.o c-bits/gc.c

c-bits/types.o: c-bits/types.c c-bits/types.h
	$(CLANG) -c -o c-bits/types.o c-bits/types.c

clean:
	rm -rf c-bits/*.o tests/output/*.o tests/output/*.s tests/output/*.dSYM tests/output/*.run tests/output/*.log tests/output/*.*result $(ASGN)-$(COMPILER).tgz

distclean: clean
	stack clean
	rm -rf .stack-work

tags:
	hasktags -x -c lib/

turnin:
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream git@github.com:$(ORG)/$(ASGN)-$(COMPILER).git

update:
	git pull upstream master



# aliases

INPUTS  := $(patsubst tests/input/%.$(EXT),%,$(wildcard tests/input/*.$(EXT)))
ASMS    := $(patsubst %,%-s,$(INPUTS))
OBJS    := $(patsubst %,%-o,$(INPUTS))
RUNS    := $(patsubst %,%-run,$(INPUTS))
RESULTS := $(patsubst %,%-result,$(INPUTS))

$(ASMS): %-s: tests/output/%.s
	cat $<
$(OBJS): %-o: tests/output/%.o
$(RUNS): %-run: tests/output/%.run
$(RESULTS): %-result: tests/output/%.result
	cat $<

FORCE:
