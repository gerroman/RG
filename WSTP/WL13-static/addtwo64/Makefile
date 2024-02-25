MLINKDIR=/opt/WolframEngine/13.0/SystemFiles/Links/MathLink/DeveloperKit/Linux-x86-64
SYS=Linux-x86-64
CADDSDIR=${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-m64
MLLIB=ML64i4

INCDIR=${CADDSDIR}
LIBDIR=${CADDSDIR}

LIBUUIDDIR=/opt/WolframEngine/13.0/SystemFiles/Libraries/Linux-x86-64

MPREP=${CADDSDIR}/mprep
RM=rm

BINARIES=addtwo64.bin

all: $(BINARIES)

addtwo64tm.c: addtwo64.tm
	${MPREP} $? -o $@

.c.o:
	${CC} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

addtwo64.bin: addtwo64tm.o addtwo64.o
	${CC} ${EXTRA_CFLAGS} -I${INCDIR} addtwo64tm.o addtwo64.o -L${LIBDIR} -static -l${MLLIB} -L${LIBDIR} -lm -lpthread -lrt -lstdc++ -ldl -L${LIBUUIDDIR} -luuid -o $@

clean:
	@ ${RM} -rf *.o *tm.c $(BINARIES)

test: addtwo64.bin
	./test.wl

link: addtwo64.bin
	./test.sh

