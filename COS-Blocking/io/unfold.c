#include <stdio.h>
#include <stdlib.h>

int main(int argc,char **argv)
{
  FILE *fp;
  int next_char;

/* read from standard input */
  if (argc == 1)
    fp=stdin;
/* read from a file */
  else {
    if ( (fp=fopen(argv[1],"r")) == NULL) {
	fprintf(stderr,"Error opening %s\n",argv[1]);
	exit(1);
    }
  }

/* read the stream one character at a time */
  while ( (next_char=getc(fp)) != EOF) {
/* filter out linefeeds */
    if (next_char != 0xa)
	putc(next_char,stdout);
  }
}
