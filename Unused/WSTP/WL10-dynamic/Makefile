.PHONY: all test info clean
SUBDIRS = addtwo64 addtwo64w addtwo32 addtwo32w
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
	echo ".......... WL10-dynamic tests .........."

%test: %
	$(MAKE) -C $< test
