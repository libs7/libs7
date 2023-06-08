.PHONY: test clean

test:
	@echo starting test
	@valgrind ../mustach json must > resu.last 2> vg.last
	@sed -i 's:^==[0-9]*== ::' vg.last
	@diff -w resu.ref resu.last && echo "result ok" || echo "ERROR! Result differs"
	@awk '/^ *total heap usage: .* allocs, .* frees,.*/{if($$4-$$6)exit(1)}' vg.last || echo "ERROR! Alloc/Free issue"
	@echo

clean:
	rm -f resu.last vg.last

