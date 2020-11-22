SUBDIRS = BaseUtils Presentation Calculation Notation Kinematics Traces
all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: all $(SUBDIRS)
