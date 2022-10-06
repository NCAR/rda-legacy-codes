/*
** File: taperead.c
**
** Author: Bob Dattore
**         NCAR/DSS
**         dattore@ucar.edu
**         (303) 497-1825
**
** Latest Revision:  1 Mar 99
**
** Purpose: to provide a UNIX-like utility for extracting records from a tape
**          device
**
** Usage: for information on how to use this utility, create an executable and
**        run it with no command-line arguments
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <strings.h>
#include <math.h>

#define MAX_LENGTH 32768

struct ArgList {
  size_t num_blocks,num_files;
  char *device,*filename;
  unsigned int add_newline;
} args;

void parseArgs(int argc,char **argv);
void readFromTape(void);
void incrementExtension(char *ext,size_t ext_length);

int main(int argc,char **argv)
{
  if (argc < 3) {
    fprintf(stderr,"\nusage: %s [options...] device file\n\n",argv[0]);
    fprintf(stderr,"description:\n");
    fprintf(stderr," %s extracts data from a tape device and writes it a disk "
      "file(s).\n",argv[0]);
    fprintf(stderr," The user may specify a specific number of blocks *or* "
      "files to read\n");
    fprintf(stderr," before %s terminates.  If neither -b nor -f are specified,"
      " %s\n",argv[0],argv[0]);
    fprintf(stderr," wll read from the current tape position to the end of the "
      "tape and\n");
    fprintf(stderr," terminate.  For -b, %s will read, at most, the specified "
      "number of\n",argv[0]);
    fprintf(stderr," blocks, terminating on an EOF if it is encountered.  For "
      "-f, %s will\n",argv[0]);
    fprintf(stderr," write, at most, the specified number of files, terminating"
      " on an EOT.\n\n");
    fprintf(stderr," \"device\" refers to the tape device name and \"file\" "
      "refers to the output\n");
    fprintf(stderr," file name.  For multiple file output, the file number, "
      "from the current\n");
    fprintf(stderr," tape position, is appended as an extension to \"file\".\n"
      "\n");
    fprintf(stderr," terminates with status = 0 for successful termination "
      "and status = 1\n");
    fprintf(stderr," for various errors.\n\n");
    fprintf(stderr," NOTES:\n");
    fprintf(stderr,"   1) for multiple-file extraction, specify the device as "
      "no-rewind so that\n");
    fprintf(stderr,"      the tape will not rewind after each EOF.  Instead, "
      "use \"mt\", for\n");
    fprintf(stderr,"      example, to rewind the tape before using %s.\n\n",
      argv[0]);
    fprintf(stderr,"   2) %s has been tested on a Sun running Solaris and an "
      "SGI running\n");
    fprintf(stderr,"      IRIX - please report any problems to dattore@ucar.edu"
      "\n\n");
    fprintf(stderr,"   3) system-specific information:\n");
    fprintf(stderr,"      SGI-IRIX:  be sure to use a variable block size "
      "device for reading\n");
    fprintf(stderr,"                 (see the man page for \"tps\")\n\n");
    fprintf(stderr,"options:\n");
    fprintf(stderr," -b num|-f num   read num tape blocks/files and quit\n");
    fprintf(stderr," -n              append a newline character to "
      "each tape record\n");
    exit(1);
  }

/* parse command line arguments */
  parseArgs(argc,argv);

/* read the tape */
  readFromTape();

  exit(0);
}

void parseArgs(int argc,char **argv)
{
  size_t next=1;

  args.num_blocks=0;
  args.num_files=0x7fff;
  args.add_newline=0;

  while (strncmp(argv[next],"-",1) == 0) {
    if (strcmp(argv[next],"-b") == 0) {
	if (args.num_files > 1) {
	  fprintf(stderr,"Error: options -b and -f are mutually exclusive\n");
	  exit(1);
	}
	next++;
	if (argv[next][0] < '0' || argv[next][0] > '9') {
	  fprintf(stderr,"Error: invalid block count %s\n",argv[next]);
	  exit(1);
	}
	args.num_blocks=atoi(argv[next]);
    }
    else if (strcmp(argv[next],"-f") == 0) {
	if (args.num_blocks > 0) {
	  fprintf(stderr,"Error: options -b and -f are mutually exclusive\n");
	  exit(1);
	}
	next++;
	if (argv[next][0] < '0' || argv[next][0] > '9') {
	  fprintf(stderr,"Error: invalid block count %s\n",argv[next]);
	  exit(1);
	}
	args.num_files=atoi(argv[next]);
    }
    else if (strcmp(argv[next],"-n") == 0)
	args.add_newline=1;
    else {
	fprintf(stderr,"Error: invalid flag %s\n",argv[next]);
	exit(1);
    }
    next++;
  }

  if (next == argc) {
    fprintf(stderr,"Error: no input device and output file specified\n");
    exit(1);
  }
  else {
    args.device=(char *)malloc(strlen(argv[next])+1);
    strcpy(args.device,argv[next]);
    if (strncmp(args.device,"/dev",4) != 0)
	fprintf(stderr,"Warning: %s may not be a valid device\n",args.device);
    next++;
  }
  if (next == argc) {
    fprintf(stderr,"Error: no output file specified\n");
    exit(1);
  }
  else {
    args.filename=(char *)malloc(strlen(argv[next])+1);
    strcpy(args.filename,argv[next]);
  }
}

void readFromTape(void)
{
  FILE *device,*file;
  int bytes_read;
  size_t n,blocks_read;
  unsigned char buf[MAX_LENGTH];
  char *filename=NULL,*ext;
  size_t ext_length;

  n=10;
  ext_length=2;
  while (args.num_files >= n) {
    ext_length++;
    n*=10;
  }
  ext=(char *)malloc(ext_length+1);
  ext[0]='.';
  for (n=1; n < ext_length; ext[n++]='0');
  ext[ext_length]='\0';

  printf("Reading from %s...\n",args.device);

/* try to read the specified number of files */
  for (n=0; n < args.num_files; n++) {
/* open the device */
    if ( (device=fopen(args.device,"r")) == NULL) {
	fprintf(stderr,"Error: unable to open device %s\n",args.device);
	exit(1);
    }
    blocks_read=0;

/* read from the tape until the EOF is reached */
    while ( (bytes_read=read(fileno(device),buf,MAX_LENGTH)) > 0) {
	blocks_read++;

	if (blocks_read == 1) {
/* open the output file */
	  if (args.num_files == 1) {
	    if ( (file=fopen(args.filename,"w")) == NULL) {
		fprintf(stderr,"Error: unable to open output file %s\n",
              args.filename);
		exit(1);
	    }
	  }
	  else {
/* for multiple file output, get the extension for the current filename */
	    incrementExtension(ext,ext_length);
	    if (filename != NULL) free(filename);
	    filename=(char *)malloc(strlen(args.filename)+ext_length+1);
	    strcpy(filename,args.filename);
	    strcat(filename,ext);
	    if ( (file=fopen(filename,"w")) == NULL) {
		fprintf(stderr,"Error: unable to open output file %s\n",filename);
		exit(1);
	    }
	  }
	}

/* add the newline character if this option was chosen */
	if (args.add_newline) {
	  buf[bytes_read]=0xa;
	  bytes_read++;
	}

/* write a record to the output file */
	fwrite(buf,1,bytes_read,file);
	if (args.num_blocks > 0 && blocks_read == args.num_blocks)
	  break;
    }

    fclose(device);
    if (blocks_read > 0) {
	fclose(file);
	if (args.num_files == 1)
	  printf("  Output file: %s  Records written: %d\n",args.filename,
          blocks_read);
	else
	  printf("  Output file: %s  Records written: %d\n",filename,blocks_read);
	if (filename != NULL) {
	  free(filename);
	  filename=NULL;
	}
    }
    else
	break;
  }
}

void incrementExtension(char *ext,size_t ext_length)
{
  size_t n;

  for (n=ext_length-1; n > 0; n--) {
    ext[n]++;
    if (ext[n] > '9')
	ext[n]='0';
    else
	break;
  }
}
