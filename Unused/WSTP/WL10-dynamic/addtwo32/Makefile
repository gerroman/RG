# This makefile can be used to build all or some of the sample
# programs.  To build all of them, use the command
# 'make all'.  To build one, say addtwo32, use the command
# 'make addtwo32'.

# Portions of this makefile require the use of GNU make.
# see http://www.gnu.org/software/make for more information.

MLINKDIR=/opt/Mathematica/SystemFiles/Links/MathLink/DeveloperKit/Linux-x86-64
SYS=Linux-x86-64
CADDSDIR=${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-m32
MLLIB=ML32i4

INCDIR=${CADDSDIR}
LIBDIR=${CADDSDIR}

MPREP=${CADDSDIR}/mprep
RM=rm

BINARIES=addtwo32.bin

all: $(BINARIES)

addtwo32tm.c: addtwo32.tm
	${MPREP} $? -o $@

.c.o:
	${CC} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

addtwo32.bin: addtwo32tm.o addtwo32.o
	${CC} ${EXTRA_CFLAGS} -I${INCDIR} addtwo32tm.o addtwo32.o -L${LIBDIR} -l${MLLIB} -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@

clean:
	@ ${RM} -rf *.o *tm.c $(BINARIES)

test: addtwo32.bin
	./test.wl
