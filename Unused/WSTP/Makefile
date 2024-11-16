SUBDIRS = WL10-dynamic WL13-dynamic WL13-static
SUBDIRSCLEAN=$(addsuffix clean,$(SUBDIRS))
SUBDIRSALL=$(addsuffix all,$(SUBDIRS))
SUBDIRSTEST=$(addsuffix test,$(SUBDIRS))

all: $(SUBDIRSALL)

%all: %
	$(MAKE) -C $< all

clean: $(SUBDIRSCLEAN)

%clean: %
	$(MAKE) -C $< clean

test: $(SUBDIRSTEST)

%test: %
	$(MAKE) -C $< test
