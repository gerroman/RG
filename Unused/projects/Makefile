
.PHONY: check all clean wipe tar run diff test

project=my-project
scripts=./src/init.wl ./src/make-directories.wl
inits=./src/init-definitions.wl ./src/init-paths.wl
tests=./tests/test.wl
external=

archive_path=${Projects}/data/${project}
archive_name=${archive_path}/${project}
archive_content=./src/init-*.wl ./run/* ./description.html

all: check
	wolfram -script ./src/make-directories.wl
	wolfram -script ./src/script.wl

clean:
	-find ./run -mindepth 1 -maxdepth 5 -type f -exec rm -v {} \;
	-rm ./run.log
	-rm ./description.html

wipe: clean
	-find ./src -type f -exec rm -v {} \;
	-find ./tests -type f -exec rm -v {} \;
	-rm -rf ./run/*;
	-rm ./run.log
	-rm ./description.html
	emacs --batch \
				--eval "(require 'org)" \
				--eval "(setq org-src-preserve-indentation t)" \
				--eval "(add-to-list 'load-path \"~/.emacs.d/lisp\")" \
				--eval "(load \"wl\")" \
				--eval "(org-babel-tangle-file \"${project}.org\")"

check: ${scripts} ${inits}
	wolfram -script RG/SyntaxChecker/check.wl $^

./run.log: ${scripts} ${inits} ${external}
	make clean
	make all 2>&1 | tee ./run.log
	cat ./run.log | sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,2};?)?)?[mGK]//g" >./run/run.txt

test: ./run.log
	wolfram -script RG/SyntaxChecker/check.wl ${tests}
	@for t in `find ./tests -type f -print | sort`; do printf "$$t\n" && wolfram -script "$$t"; done

diff:
	-tar --diff -f ${archive_name}.tar .

description.html: ./run.log
	emacs --batch \
				--eval "(require 'org)" \
				--eval "(setq org-src-preserve-indentation t)" \
				--eval "(add-to-list 'load-path \"~/.emacs.d/lisp\")" \
				--eval "(load \"wl\")" \
				${project}.org \
				--eval "(org-export-as-html 1)"&& \
		mv ${project}.html $@

tar: ${archive_name}.tar

${archive_name}.tar: ${archive_content}
	@ [[ -d ${archive_path} ]] || mkdir -v ${archive_path}
	@- [[ -f ${archive_name}.tar ]] && (printf "[warning]: '${archive_name}.tar' does exists ... \n" \
		&& gzip --stdout ${archive_name}.tar >"${archive_name}-`date +%Y-%m-%d_%H%M%S`.tar.gz")

	tar --create -f ${archive_name}.tar ${archive_content}
