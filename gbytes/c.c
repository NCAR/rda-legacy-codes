#include <stdio.h>

/* Tools for storage/retrieval of arbitrary size bytes from 32-bit words
    (note - this version is not currently (6/30/88) described in the
    gbytes document)

    gbytes(p,u,q,b,s,n)
    gbyte (p,u,q,b)
    sbytes(p,u,q,b,s,n)
    sbyte (p,u,q,b)

             q >= 0     number of bits to be skipped preceeding first byte in p
      0 <    b < sword  byte size
             s >= 0     number of bits to be skipped between bytes
             n >= 0     number of bytes to be packed/unpacked

    gbytes unpacks n b bit bytes from p into u, starting by skipping
         q bits in p, then skipping s bits between bytes.
    gbyte unpacks one such byte.
    sbytes   packs n b bit bytes from u into p, starting by skipping
         q bits in p, then skipping s bits between bytes.
    sbyte  packs one such byte. */
# define SWORD 32                              /* Word size in bits */
# define MASK 0xffffffff                       /* Mask of sword bits */
# define G1BYTE(p,q,b) ((b==32 ? MASK : ~(MASK<<b)) & (p>>(SWORD-(q+b))))
                                               /* Get 1 word contained byte */
# define MASK1(q,b) (b==32 ? MASK : (~(MASK<<b)<<(SWORD-(q+b))))
                                               /* Mask of sword bits */
/* Common code for gbytes, sbytes */
void gsbytes(long p[],long u[],long q,long b,long s,long n,
void (*gsbyte)(long p[],long *u,long q,long b))
{       long jp,ju;
        jp = 0;
        for (ju = 0; ju < n; ++ju) {
                 (*gsbyte)(&p[jp],&u[ju],q,b);
                 q += b + s;
                 jp += q/SWORD;
                 q %= SWORD;
        }
}
void gbyte(long p[],long *u,long q,long b)
{
        long qb,j,lb;

        if (q >= SWORD) {
                 j = q/SWORD; /* number of words offset */
                 q %= SWORD;  /* odd bits of offset     */
        }
        else {
                 j=0;
        }
        qb = q + b;
        if (qb > SWORD) {
                 qb = SWORD - q;
                 b -= qb;
                 lb = (G1BYTE(p[j],q,qb)) << b;
                 q = 0;
                 j++;  /* increment to next word */
        }
        else lb = 0;
        *u = lb + (G1BYTE(p[j],q,b));
}
void gbytes(long p[],long u[],long q,long b,long s,long n)
{
        gsbytes(p,u,q,b,s,n,gbyte);
}
void sbyte(long p[],long *u,long q,long b)
{
        long qb,j,rb;

        if (q >= SWORD) {
                 j = q / SWORD;    /* number of words offset */
                 q %= SWORD;       /* odd bit offset         */
        }
        else {
                 j = 0;
        }
        qb = q + b;
        if (qb > SWORD) {
                 qb = SWORD - q;
                 q = SWORD - b;
                 b -= qb;
                 p[j] = ((p[j] >> qb) << qb) + (G1BYTE(*u,q,qb));
                 q = 0;
                 j++;  /* point to next word */
        }
        rb = G1BYTE(*u,SWORD-b,b);
        p[j] = (p[j] & ~MASK1(q,b)) + (rb << SWORD-(b+q));
}
void sbytes(long p[],long u[],long q,long b,long s,long n)
{
        gsbytes(p,u,q,b,s,n,sbyte);
}
