SIMPLC_FLAGS =
SIMPLC = stack run simplc --
RUNTIME = runtime
GC = $(RUNTIME)/libgc.a
TEST_DIR = test-suite
TEST_BIN = $(TEST_DIR)/bin
TEST_SRCS = $(wildcard $(TEST_DIR)/*.spl)

test: $(TEST_SRCS:$(TEST_DIR)/%.spl=$(TEST_BIN)/%)

$(TEST_BIN)/%.o: $(TEST_DIR)/%.spl $(TEST_BIN) $(RUNTIME)
	$(SIMPLC) $< -o $@ $(SIMPLC_FLAGS)

$(TEST_BIN)/%: $(TEST_BIN)/%.o
	clang -g -pthread $< $(GC) -o $@ -lm

$(TEST_BIN):
	mkdir -p $@

$(RUNTIME): $(GC)

$(GC):
	make -C $(RUNTIME) libgc.a

clean:
	rm -rf $(TEST_BIN)

.PHONY: test clean
