.PHONY: test clean

test-custom-write: test-custom-write.c ../mustach-json-c.h ../mustach-json-c.c ../mustach-wrap.c ../mustach.h ../mustach.c
	@echo building test-custom-write
	$(CC) $(CFLAGS) $(LDFLAGS) -g -o test-custom-write test-custom-write.c  ../mustach.c  ../mustach-json-c.c ../mustach-wrap.c -ljson-c

test: test-custom-write
	@echo starting test
	@valgrind ./test-custom-write json -U must -l must -x must > resu.last 2> vg.last
	@sed -i 's:^==[0-9]*== ::' vg.last
	@diff -w resu.ref resu.last && echo "result ok" || echo "ERROR! Result differs"
	@awk '/^ *total heap usage: .* allocs, .* frees,.*/{if($$4-$$6)exit(1)}' vg.last || echo "ERROR! Alloc/Free issue"
	@echo

clean:
	rm -f resu.last vg.last test-custom-write

