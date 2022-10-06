/*
 * To read variable length records from 1/2 inch tape and put an new_line
 * at the end of each physical record
 */
#include <stdio.h>
#include <errno.h>
#define MAXBUF 32768            /* maximum tape physical block size */

extern char *index();
extern int errno;
char buff[MAXBUF];
FILE *tapedev;
FILE *outdev;
long recno, nrec, neof, filen, nrecl, nrece, numb;
int sys_nerr;
/*char *sys_errlist[];*/

main ()
{
    char outfl[64],infl[64];
    int n, k, nmx, nmn,iadd;
    printf ("A binary copy will put an exact copy of bytes from \n");
    printf (" the tape into a disk file.                        \n");
    printf ("An ASCII copy will insert a line feed after each   \n");
    printf (" tape block as it is copied to the disk file.      \n");
    printf (" \nEnter 0 for a binary copy or 1 for an ASCII copy  \n");
    scanf ("%8id", &iadd);
    if (iadd < 0) iadd = 0;
    if (iadd > 1) iadd = 1;

    printf ("Enter input tape drive. </dev/nrst0> \n");
    scanf ("%63s", infl);
    if ((tapedev = fopen (infl, "r")) == NULL) {
        fprintf (stderr, "t2d: cannot open input %s\n", infl);
	exit(3);
    }
    filen = 0;

    for (;;) {
    printf ("\nCopy will stop on EOF or block count. \n");
    printf ("\nEnter number of blocks to copy.\n");
    printf ("OR \n");
    printf ("Enter 0 to copy to next EOF, -1 to quit)\n");
    scanf ("%16ld", &nrecl);
    if(nrecl<0) exit(0);
    printf ("Enter output disk filename. (WARNING, overwrites existing files)\n");
    scanf ("%63s", outfl);
    fclose (outdev);
    if ((outdev = fopen (outfl, "w+")) == NULL) {
       fprintf (stderr, "t2d: cannot open %s\n", outfl);
       exit (3);
    }
    if(nrecl>0)
      printf ("Copying %ld blocks ---  \n", nrecl);
    else
      printf ("Copying one file ---  \n");
    nrec=0;
    nrece=0;
    recno = 0;
    nmx=0;
    nmn=MAXBUF;
    numb=0;

     while ((n = read (fileno (tapedev), buff, sizeof buff)) != 0) {

	buff[n]='\n';
	if (write (fileno (outdev), buff, n + iadd) <= 0) {
            fprintf (stderr, "t2d: file write error #%d in %s\n",
            errno, outfl);
            exit (2);
        }
	if (n < 0 ) {
	      printf ("Tape read error %d in block %ld\n", n, recno);
	      nrece ++;
	      if(nrece > 10) exit(3);
	}
	if(n > nmx) nmx = n ;
	if(n < nmn) nmn = n ;
	numb = numb + n;
        recno ++;
        nrec ++;
	if ( nrecl > 0 & nrec >= nrecl ) {
	    printf ("\n");
	    printf ("Blocks, bytes copied min, max  %d %d %d %d\n",
		nrec,numb,nmn,nmx);
	    break;
	}
     }

	printf ("\n");
	if (n == 0)
            if (recno == 0)
		  printf ("File %d is empty\n", filen+1);
            else {
		  printf ("EOF in file %d,  blocks %d\n", filen+1, nrec);
		  printf ("Bytes, Min, Max %d %d %d \n",numb,nmn,nmx);
                  nrec=0;
                  filen ++;
                 }
    }
}
