MLINKDIR=/opt/Mathematica/SystemFiles/Links/MathLink/DeveloperKit/Linux-x86-64
SYS=Linux-x86-64
CADDSDIR=${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-m64
MLLIB=ML64i4

INCDIR=${CADDSDIR}
LIBDIR=${CADDSDIR}

MPREP=${CADDSDIR}/mprep
RM=rm

BINARIES=addtwo64.bin

all: $(BINARIES)

addtwo64tm.c: addtwo64.tm
	${MPREP} $? -o $@

.c.o:
	${CC} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

addtwo64.bin: addtwo64tm.o addtwo64.o
	${CC} ${EXTRA_CFLAGS} -I${INCDIR} addtwo64tm.o addtwo64.o -L${LIBDIR} -l${MLLIB} -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

clean:
	@ ${RM} -rf *.o *tm.c $(BINARIES)

test: addtwo64.bin
	./test.wl
