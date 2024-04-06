
.PHONY: check all clean wipe tar run diff tests

project=project
scripts=./src/init.wl ./src/script.wl
inits=./src/init-definitions.wl ./src/init-paths.wl
external=

archive_name=./data/${project}
archive_content=./src/init-definitions.wl ./src/init-paths.wl ./run/*

all: ./run.log

run: check
	wolfram -script ./src/script.wl

clean:
	-find ./run -type f -exec rm -v {} \;

wipe: clean
	-find ./src -type f -exec rm -v {} \;

check: ${scripts} ${inits}
	wolfram -script RG/SyntaxChecker/check.wl $^

./run.log: ${scripts} ${inits} ${external}
	make clean
	make run 2>&1 | tee ./run.log
	cat ./run.log | sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,2};?)?)?[mGK]//g" >./run/run.txt

tests: all
	wolfram -script ./tests/test.wl

diff:
	-tar --diff -f ${archive_name}.tar .

tar: ${archive_name}.tar

${archive_name}.tar: ${archive_content} ./run.log
	-[[ -f ${archive_name}.tar ]] && (printf "[warning]: '${archive_name}.tar' does exists ... \n" \
		&& gzip --stdout ${archive_name}.tar >"${archive_name}-`date +%Y-%m-%d_%H%M%S`.tar.gz")

	tar --create -f ${archive_name}.tar ${archive_content}
