# Makefile for 'cos utilities' on a Linux system
#
# to make the utilities, use the following command line:
#   % make cosutils
#
COMPILER = g++
OPTIONS = -std=c++11 -I$(INCLUDEDIR)
INCLUDEDIR = ./include
SOURCEDIR = ./source
JASPERLIBDIR = 
JASPERINCLUDEDIR = 
#
cosutils: $(SOURCEDIR)/cosfile.cpp $(SOURCEDIR)/cossplit.cpp $(SOURCEDIR)/cosconvert.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/bfstream.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/buffer.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/checksum.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/craystream.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/datetime.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/f77stream.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/floatutils.cpp
	$(COMPILER) $(OPTIONS) -D __cosutils -c $(SOURCEDIR)/grib.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/grid.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/iodstream.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/rptoutstream.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/strutils.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/tempfile.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/unixutils.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/vbsstream.cpp
	$(COMPILER) $(OPTIONS) -c $(SOURCEDIR)/xtox.cpp
	$(COMPILER) $(OPTIONS) -o cosfile $(SOURCEDIR)/cosfile.cpp bfstream.o craystream.o strutils.o unixutils.o tempfile.o xtox.o
	$(COMPILER) $(OPTIONS) -o cossplit $(SOURCEDIR)/cossplit.cpp bfstream.o craystream.o strutils.o unixutils.o tempfile.o xtox.o
	$(COMPILER) $(OPTIONS) -o cosconvert $(SOURCEDIR)/cosconvert.cpp bfstream.o buffer.o checksum.o craystream.o datetime.o f77stream.o grib.o grid.o iodstream.o rptoutstream.o strutils.o floatutils.o tempfile.o vbsstream.o xtox.o
	rm bfstream.o buffer.o checksum.o craystream.o datetime.o f77stream.o floatutils.o grib.o grid.o iodstream.o rptoutstream.o strutils.o tempfile.o unixutils.o vbsstream.o xtox.o
	-@ echo ""
	-@ echo "utilities 'cosfile', 'cossplit', and 'cosconvert' successfully built"
