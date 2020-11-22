all:
	find . -name *.wlt -print0 | xargs -0L1 ./run_test.wl

.PHONY: all
