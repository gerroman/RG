.PHONY: all test info clean
SUBDIRS = addtwo64 addtwo64w
SUBDIRSCLEAN=$(addsuffix clean,$(SUBDIRS))
SUBDIRSALL=$(addsuffix all,$(SUBDIRS))
SUBDIRSTEST=$(addsuffix test,$(SUBDIRS))

all: info $(SUBDIRSALL)

%all: %
	$(MAKE) -C $< all

clean: info $(SUBDIRSCLEAN)

%clean: %
	$(MAKE) -C $< clean

test: info $(SUBDIRSTEST)

info:
	echo ".......... WL13-dynamic tests .........."

%test: %
	$(MAKE) -C $< test
