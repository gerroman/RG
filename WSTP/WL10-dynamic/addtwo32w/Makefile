MLINKDIR=/opt/Mathematica/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64
CADDSDIR=${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-m32
MLLIB=WSTP32i4

INCDIR=${CADDSDIR}
LIBDIR=${CADDSDIR}

MPREP=${CADDSDIR}/wsprep
RM=rm

BINARIES=addtwo32w.bin

all: $(BINARIES)

addtwo32wtm.cpp: addtwo32w.tm
	${MPREP} $? -o $@

.cpp.o:
	${CXX} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

addtwo32w.bin: addtwo32wtm.o addtwo32w.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} addtwo32wtm.o addtwo32w.o -L${LIBDIR} -l${MLLIB} -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

clean:
	@ ${RM} -rf *.o *tm.cpp $(BINARIES)

test: addtwo32w.bin
	env LD_LIBRARY_PATH=/opt/Mathematica/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions ./test.wl
