compile: runtime.ll

%.ll: %.c %.h
	clang -S -emit-llvm $< -o $@

clean:
	rm -f *.ll
