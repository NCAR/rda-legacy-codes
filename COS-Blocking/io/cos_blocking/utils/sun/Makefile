# Makefile for 'cos utilities' on a Sun/Solaris system
#
# to make the utilities, use the following command line:
#   % make -f Makefile.cosutils
#
COMPILER = cc
OPTIONS = -xO3
SOURCEDIR = ./source
#
cosutils: $(SOURCEDIR)/cosfile.c $(SOURCEDIR)/cossplit.c $(SOURCEDIR)/cosconvert.c
	$(COMPILER) $(OPTIONS) -o cosfile $(SOURCEDIR)/cosfile.c
	$(COMPILER) $(OPTIONS) -o cossplit $(SOURCEDIR)/cossplit.c
	$(COMPILER) $(OPTIONS) -o cosconvert $(SOURCEDIR)/cosconvert.c
