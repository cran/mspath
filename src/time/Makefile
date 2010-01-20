# Makefile for a debug test of the mspath system

# rules for generation of dependency files
# must be in this file for the expansion to work
CPPDEPENDENCY =  -MMD -MF $(DEPDIR)/$(*F).d -MP

# make implicit rules work right
CXXFLAGS += $(MAINCXXFLAGS) $(TESTCXXFLAGS) $(CPPDEPENDENCY) -O2


CXXFLAGS += 
LDFLAGS += 

include $(TRUESRCDIR)/programFiles.mk
include $(TESTSRCDIR)/testFiles.mk

# Remove stuff that needs real (not stub) R or the boost test framework
MAINOBJS := $(filter-out mspathR.o NodeFactoryTester.o, $(MAINOBJS))

# GNU make trick to pick up prerequisites above us
# because this might include .., don't use 'test' as an output file
VPATH = $(TRUESRCDIR):$(TESTSRCDIR)


# tests
.PHONY : build clean runprofile

# The profile also requires an input file,
# but it is the top-level make's responsibility to see it is there.
profile: profile.o $(MAINOBJS) $(RSTUBOBJS)
	../libtool --mode=link --tag=CXX $(CXX) -v -o $@ $^ $(LDFLAGS) $(RLIB) $(MAINLDFLAGS)

runprofile: profile
	@echo $(DYLD_IMAGE_SUFFIX) "image suffix for OS X"
	./profile $(TESTARGS)


clean:
	-rm *~
	-rm $(MAINOBJS) $(TESTOBJS) $(RSTUBOBJS)
	-rm profile profile.o

-include $(addprefix $(DEPDIR)/, $(MAINOBJS:.o=.d) $(TESTOBJS:.o=.d) $(RSTUBOBJS:.o-.d))
