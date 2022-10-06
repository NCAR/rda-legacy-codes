# Makefile for 'craylib' on an IBM/AIX system
#
# to make the libraries, use the following command line:
#   % make -f Makefile.craylib
#
CC_COMPILER = xlC
CC_OPTIONS = -c -O2 -q64
F_COMPILER = f77
F_OPTIONS = -c -q64
SOURCEDIR = ./source
INCLUDEDIR = ./include
LIBDIR = .
#
craylib: $(SOURCEDIR)/bfstream.cpp $(SOURCEDIR)/checksum2.cpp $(SOURCEDIR)/craystream.cpp $(SOURCEDIR)/craywrap.cpp $(SOURCEDIR)/gbytes_.cpp $(SOURCEDIR)/round.cpp $(SOURCEDIR)/rptinwrap.cpp $(SOURCEDIR)/rptoutstream.cpp $(SOURCEDIR)/sbytes_.cpp $(SOURCEDIR)/crayopen.f $(SOURCEDIR)/rptopen.f
	rm -rf *.o
	$(CC_COMPILER) $(CC_OPTIONS) $(SOURCEDIR)/bfstream.cpp $(SOURCEDIR)/checksum2.cpp $(SOURCEDIR)/craystream.cpp $(SOURCEDIR)/craywrap.cpp $(SOURCEDIR)/gbytes_.cpp $(SOURCEDIR)/round.cpp $(SOURCEDIR)/rptinwrap.cpp $(SOURCEDIR)/rptoutstream.cpp $(SOURCEDIR)/sbytes_.cpp -I$(INCLUDEDIR)
	$(F_COMPILER) $(F_OPTIONS) $(SOURCEDIR)/crayopen.f $(SOURCEDIR)/rptopen.f
	ar -X64 rv $(LIBDIR)/libcraylib.a *.o
	rm -rf *.o
