C_FLAGS := -O2 -Ibdwgc/include

compile: bdwgc runtime.ll

bdwgc:
	git submodule update --init

bdwgc/Makefile:
	cd bdwgc && \
		./autogen.sh && \
		./configure --enable-static=yes \
			--enable-shared=no

bdwgc/.libs/libgc.a: bdwgc bdwgc/Makefile
	make -j -C bdwgc

libgc.a: bdwgc/.libs/libgc.a
	cp $^ $@

%.ll: %.c %.h
	clang-9 $(C_FLAGS) -S -emit-llvm $< -o $@

clean:
	rm -f *.ll libgc.a
	make -C bdwgc clean

.PHONY: compile clean bdwgc
