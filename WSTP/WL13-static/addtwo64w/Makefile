MLINKDIR=/opt/WolframEngine/13.0/SystemFiles/Links/WSTP/DeveloperKit/Linux-x86-64
CADDSDIR=${MLINKDIR}/CompilerAdditions
EXTRA_CFLAGS=-m64
MLLIB=WSTP64i4

INCDIR=${CADDSDIR}
LIBDIR=${CADDSDIR}

LIBUUIDDIR=/opt/WolframEngine/13.0/SystemFiles/Libraries/Linux-x86-64

MPREP=${CADDSDIR}/wsprep
RM=rm

BINARIES=addtwo64w.bin

all: $(BINARIES)

addtwo64wtm.cpp: addtwo64w.tm
	${MPREP} $? -o $@

.cpp.o:
	${CXX} -c ${EXTRA_CFLAGS} -I${INCDIR} $<

addtwo64w.bin: addtwo64wtm.o addtwo64w.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} addtwo64wtm.o addtwo64w.o -L${LIBDIR} -static -l${MLLIB} -lm -lpthread -lrt -lstdc++ -L${LIBUUIDDIR} -ldl -luuid -o $@

clean:
	@ ${RM} -rf *.o *tm.cpp $(BINARIES)

test: addtwo64w.bin
	./test.wl

link: addtwo64w.bin
	./test.sh
