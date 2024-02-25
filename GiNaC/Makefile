MLINKDIR=/opt/Mathematica/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64
CADDSDIR=${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-m64
MLLIB=WSTP64i4

INCDIR=${CADDSDIR}
LIBDIR=${CADDSDIR}

MPREP=${CADDSDIR}/wsprep
RM=rm

BINARIES=G.bin

all: $(BINARIES)

Gtm.cpp: G.tm
	${MPREP} $? -o $@

.cpp.o:
	${CXX} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

G.bin: Gtm.o G.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} Gtm.o G.o -L${LIBDIR} -l${MLLIB} -lm -lpthread -lrt -lstdc++ -ldl -luuid -lginac -lcln -o $@

clean:
	@ ${RM} -rf *.o *tm.cpp $(BINARIES)

test: G.bin
	env LD_LIBRARY_PATH=/opt/Mathematica/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions ./test.wl

link: G.bin
	env LD_LIBRARY_PATH=/opt/Mathematica/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions ./test.sh

