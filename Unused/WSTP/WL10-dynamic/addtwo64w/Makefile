MLINKDIR=/opt/Mathematica/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64
CADDSDIR=${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-m64
MLLIB=WSTP64i4

INCDIR=${CADDSDIR}
LIBDIR=${CADDSDIR}

MPREP=${CADDSDIR}/wsprep
RM=rm

BINARIES=addtwo64w.bin

all: $(BINARIES)

addtwo64wtm.cpp: addtwo64w.tm
	${MPREP} $? -o $@

.cpp.o:
	${CXX} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

addtwo64w.bin: addtwo64wtm.o addtwo64w.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} addtwo64wtm.o addtwo64w.o -L${LIBDIR} -l${MLLIB} -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

clean:
	@ ${RM} -rf *.o *tm.cpp $(BINARIES)

test: addtwo64w.bin
	env LD_LIBRARY_PATH=/opt/Mathematica/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions ./test.wl

link: addtwo64w.bin
	env LD_LIBRARY_PATH=/opt/Mathematica/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64/CompilerAdditions ./test.sh

