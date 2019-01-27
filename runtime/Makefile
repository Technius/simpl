compile: runtime.ll

%.ll: %.c
	clang -S -emit-llvm $< -o $@

clean:
	rm -f *.ll
