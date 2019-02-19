compile: runtime.ll

%.ll: %.c %.h
	clang-7 -S -emit-llvm $< -o $@

clean:
	rm -f *.ll
