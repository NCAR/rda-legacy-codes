# Makefile for 'craylib' on an SGI/IRIX system
#
# to make the libraries, use the following command line:
#   % make -f Makefile.craylib
#
CC_COMPILER = CC
CC_OPTIONS = -c -O3 -ptused
F_COMPILER = f77
F_OPTIONS = -c
SOURCEDIR = ./source
INCLUDEDIR = ./include
LIBDIR = .
#
craylib: $(SOURCEDIR)/bfstream.cpp $(SOURCEDIR)/checksum2.cpp $(SOURCEDIR)/craystream.cpp $(SOURCEDIR)/craywrap.cpp $(SOURCEDIR)/gbytes_.cpp $(SOURCEDIR)/round.cpp $(SOURCEDIR)/rptinwrap.cpp $(SOURCEDIR)/rptoutstream.cpp $(SOURCEDIR)/sbytes_.cpp $(SOURCEDIR)/crayopen.f $(SOURCEDIR)/rptopen.f
	rm -rf *.o
	$(CC_COMPILER) $(CC_OPTIONS) $(SOURCEDIR)/bfstream.cpp $(SOURCEDIR)/checksum2.cpp $(SOURCEDIR)/craystream.cpp $(SOURCEDIR)/craywrap.cpp $(SOURCEDIR)/gbytes_.cpp $(SOURCEDIR)/round.cpp $(SOURCEDIR)/rptinwrap.cpp $(SOURCEDIR)/rptoutstream.cpp $(SOURCEDIR)/sbytes_.cpp -I$(INCLUDEDIR)
	$(F_COMPILER) $(F_OPTIONS) $(SOURCEDIR)/crayopen.f $(SOURCEDIR)/rptopen.f
	ar rv $(LIBDIR)/libcraylib.a *.o
	rm -rf *.o
