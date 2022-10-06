/*
 * To read variable length records from 1/2 inch tape and put an new_line
 * at the end of each physical record
 */
#include <stdio.h>
#include <errno.h>
#define MAXBUF 32768            /* maximum tape physical record size */

extern char *index();
extern int errno;
char buff[MAXBUF];
FILE *tapedev;
FILE *outdev;
long recno, nrec, neof, filen, nrecl;
int sys_nerr;
/*char *sys_errlist[];*/

main ()
{
    char outfl[32],infl[32];
    int n, k;
    printf ("enter input tape drive? </dev/nrst0> \n");
    scanf ("%31s", infl);
    if ((tapedev = fopen (infl, "r")) == NULL) {
        fprintf (stderr, "t2d: cannot open input %s\n", infl);
        exit (2);
    }
    filen = 0;
    for (;;) {
    printf ("enter number of blocks to copy? \n");
    scanf ("%16ld", &nrecl);
    if(nrecl==0) exit(0);
    printf ("Copying %ld blocks.\n", nrecl);
    printf ("enter output disk filename? \n");
    scanf ("%31s", outfl);
    fclose (outdev);
    nrec=0;
    recno = 0;
reread: while ((n = read (fileno (tapedev), buff, sizeof buff)) > 0) {
        printf ("length %d in record %d\n", n, recno);
        if ( nrec == 0 ) {
            if ((outdev = fopen (outfl, "w+")) == NULL) {
               fprintf (stderr, "t2d: cannot open %s\n", outfl);
               exit (2);
             }
        }
 
        buff[n]='\n';
	if (write (fileno (outdev), buff, n+1) <= 0) {
            fprintf (stderr, "t2d: file write error #%d in %s\n",
            errno, outfl);
            exit (2);
        }
        recno ++;
        nrec ++;
	if ( nrec >= nrecl ) { exit(0);}
        }
        if (n == 0)
            if (recno == 0)
              break;
            else {
                  printf ("file %d,  records %d\n", filen+1, nrec);
                  nrec=0;
                  filen ++;
                 }
        else {
              printf ("Tape read error %d in record %ld\n", n, recno);
              if ( neof = feof(tapedev) > 0) {
                 printf ("EOF encountered, exit \n");
                 exit(1);
                 }
/*              printf (" error %s \n",sys_errlist[errno]); */
             }
       }
    fclose (tapedev);
    exit(0);
}
