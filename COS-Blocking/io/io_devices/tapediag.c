/*
 * To read variable length records from external device
 */
#include <stdio.h>
#include <errno.h>
#define MAXBUF 32768            /* maximum tape physical record size */

extern char *index();
extern int errno;
char buff[MAXBUF];
FILE *tapedev;
FILE *outdev;
long recno, nrec, neof, filen, nrecl,nfil;
int sys_nerr;
char outfl[32],infl[32],op[2];
int n, k;
/*char *sys_errlist[];*/

main ()
{

    printf ("enter input tape drive? </dev/rmt8> \n");
    scanf ("%31s", infl);
    recno = 0;
    if ((tapedev = fopen (infl, "r")) == NULL) {
        fprintf (stderr, "t2d: cannot open input %s\n", infl);
        exit (2);
    }
    printf ("enter output disk filename? \n");
    scanf ("%31s", outfl);
	if ((outdev = fopen (outfl, "w+")) == NULL) {
	   fprintf (stderr, "t2d: cannot open %s\n", outfl);
	   exit (2);
    }
 start:
    printf ("Enter Operand (?,cp,sk,fs,rw,nf,qq)? \n");
    scanf ("%2s", op);

    if (op[0] == '?') {
      printf ("cp = copy records to current output file. \n");
      printf ("sk = skip records. \n");
      printf ("fs = file skip. \n");
      printf ("rw = rewind the input tape. \n");
      printf ("nf = start a new output file. \n");
      printf ("qq = quit.\n \n");
      goto start;
    }

    if (op[0] == 'r') {
      fclose(tapedev);
      recno = 0;
       if ((tapedev = fopen (infl, "r")) == NULL) {
	   fprintf (stderr, "t2d: cannot open input %s\n", infl);
	   exit (2);
	 }
      goto start;
    }

    if (op[0] == 'n') {
      printf ("enter new output disk filename? \n");
      scanf ("%31s", outfl);
      fclose (outdev);
	  if ((outdev = fopen (outfl, "w+")) == NULL) {
	     fprintf (stderr, "t2d: cannot open %s\n", outfl);
	     exit (2);
      }
    goto start;
    }


    if (op[0] == 'c') {
    printf ("enter count of blocks to copy? \n");
    scanf ("%16ld", &nrecl);
    printf ("copying %ld blocks.\n", nrecl);
    filen = 0;
    nrec=0;
    for (;;) {
	n = read (fileno (tapedev), buff, sizeof buff);
	if (n > 0){
        recno ++;
        nrec ++;
	printf ("length %d copying record %d \n", n, nrec);
	if (write (fileno (outdev), buff, n) <= 0) {
            fprintf (stderr, "t2d: file write error #%d in %s\n",
            errno, outfl);
            exit (2);
        }
	   if ( nrec >= nrecl ) break;
		   }
	else {
	   if ( n == 0) {
	      printf ("EOF encountered at rec no %ld \n",recno);
	      break;
	     }

	 printf ("Tape read error %d in record %ld\n", n, recno);
	 break;
             }
       }
    goto start;
    }


    if (op[0] == 'f') {
    printf ("enter count of files to skip? \n");
    scanf ("%16ld", &nrecl);
    printf ("Skipping %ld files.\n", nrecl);

      nfil = 0;
    for (;;) {
      nrec=0;
    for (;;) {
	n = read (fileno (tapedev), buff, sizeof buff);
	if (n > 0){
        recno ++;
        nrec ++;
		   }
	else {
	   if ( n == 0) {
	      nfil ++;
	      printf ("EOF %ld encountered, recs = %ld \n",nfil, nrec);
	      break;
	     }

	 printf ("Tape read error %d in record %ld\n", n, recno);
	 break;
             }
       }
     if(nfil >= nrecl ) break;
       }
    goto start;
    }


    if (op[0] == 's') {
    printf ("enter count of blocks to skip? \n");
    scanf ("%16ld", &nrecl);
    printf ("Skipping %ld blocks.\n", nrecl);
    filen = 0;
    nrec=0;
    for (;;) {
	n = read (fileno (tapedev), buff, sizeof buff);
	if (n > 0){
        recno ++;
        nrec ++;
	printf ("length %d skipping record %d \n", n, nrec);
	   if ( nrec >= nrecl ) break;
		   }
	else {
	   if ( n == 0) {
	      printf ("EOF encountered at rec no %ld \n",recno);
	      break;
	     }

	 printf ("Tape read error %d in record %ld\n", n, recno);
	 break;
             }
       }
    goto start;
    }
    if (op[0] == 'q') {
    fclose (tapedev);
    fclose (outdev);
    exit(0);
    }
    goto start;
}
