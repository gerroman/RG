.PHONY: checkpaths all clean test link

MLINKDIR=${MATHEMATICA_BASE}/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64
CADDSDIR=${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-std=c++11 -m64
MLLIB=WSTP64i4

INCDIR=${CADDSDIR}
LIBDIR=${CADDSDIR}

MPREP=${CADDSDIR}/wsprep
RM=rm

BINARIES=G.bin

all: checkpaths $(BINARIES)

Gtm.cpp: G.tm
	${MPREP} $? -o $@

.cpp.o:
	${CXX} -c ${EXTRA_CFLAGS} -I${INCDIR} `pkg-config --libs --cflags ginac` $<

G.bin: Gtm.o G.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} Gtm.o G.o -L${LIBDIR} -l${MLLIB} -lm -lpthread -lrt -lstdc++ -ldl -luuid `pkg-config --libs --cflags ginac` -o $@

clean:
	@ ${RM} -rf *.o *tm.cpp $(BINARIES)

test: checkpaths G.bin
	math -script ./test.wl

link: checkpaths G.bin
	./test.sh

checkpaths: 
	@ if [[ "${MATHEMATICA_BASE}" == "" ]]; then printf "[error]: MATHEMATICA_BASE is undefined' \n\nexport MATHEMATICA_BASE=\"... path to Mathematica base directory ...\"\n\n"; exit 1; else echo "[info]: using MATHEMATICA_BASE='${MATHEMATICA_BASE}'"; fi

	@ if [[ ":${LD_LIBRARY_PATH}:" == *"CompilerAdditions"* ]]; then echo "[info]: using LD_LIBRARY_PATH='${LD_LIBRARY_PATH}'"; else printf "[error]: LD_LIBRARY_PATH does not contain 'CompilerAdditions' \n\nexport LD_LIBRARY_PATH=\"\$$LD_LIBRARY_PATH:${MATHEMATICA_BASE}/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions\"\n\n"; exit 1; fi

