# This is intended to build a text-only variant of the program in this directory, based on
# http://troydm.github.io/blog/2014/01/26/making-30-years-old-pascal-code-run-again/
# It probably won't attempt anything fancy like working out what revision the sources are
# which might result in warnings during compilation and possibly linkage, to do the
# whole job properly use lazbuild or the Lazarus IDE. MarkMLl.

# PREREQUISITE: FPC (Free Pascal Compiler), v2.6.0 but preferably v3.0.2 or above.

############

# FPCFLAGS can usefully be transcribed from the Lazarus IDE's "Show Options" output.

FPC=/usr/local/bin/fpc
FPCFLAGS=-O3 -CX -XX -k--build-id 
DEFINES=
INIFILES=../common
CPU=$(shell uname -m | sed 's/i686/i386/' | sed 's/armv7l/arm/')
OPSYS=$(shell uname -o | sed 's/GNU\/Linux/linux/')
FPCSrcDir=/usr/local/src/fpc/$(shell $(FPC) -iV)/compiler

# Note that the CPU identifier above loses potentially-useful information, but
# this was done in order to generate the same filenames emitted by Lazarus.

############

all: cms50dplus-$(CPU)-$(OPSYS)

# NOTE THAT THIS MIGHT ONLY BE A PARTIAL DEPENDENCY LIST.

$(INIFILES)/inifilesabout.o: Makefile $(INIFILES)/inifilesabout.pas

cms50dplus-$(CPU)-$(OPSYS): Makefile $(INIFILES)/inifilesabout.o ../common/locatecp210xport.pas consoleapp.pas cms50dplus.lpr 
	$(FPC) $(FPCFLAGS) $(DEFINES) -Fu$(INIFILES) -Fu../common -ocms50dplus-$(CPU)-$(OPSYS) cms50dplus.lpr 

############

clean:
	rm -f cms50dplus*.ppu cms50dplus*.o cms50dplus.or
	rm -f $(INIFILES)/inifilesabout.o $(INIFILES)/inifilesabout.ppu
                
distclean: clean
	rm -f cms50dplus-$(CPU)-$(OPSYS)

