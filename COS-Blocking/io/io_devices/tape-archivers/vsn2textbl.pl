#! /usr/local/bin/perl
#
# cat milee9712.2.list | awk -F/ '{print "msread -fBI "$3" /"$2"/"$3}'
#
# read export list from ~/datasets/ds090.0/COST
# grep VSN and file_type, # of records, size
# add number of records, size up to compare with msexport log
# write out the information in TeX table
#
$file = $ARGV[0];
#"bian2D.5";
$ext='.tex';
$htmlfile = "$file$ext";
#$htmlfile = "bian2D5.tex";
#$stape = "S26054";
$ext2='.list';
$mssl="$file$ext2";
$stape = $ARGV[1];
#----------------------------------------------------
open (MSSJOB, ">$mssl");
open (HTML, ">$htmlfile");
$h1 = '$$\vbox{';
$h2 = '\def\tablerule{\noalign{\vskip 2pt\hrule\vskip 2pt}}';
$h3 = '\halign{\hfil#\hfil&\hfil#\hfil&\hfil#\hfil&#\hfil&\hfil#\hfil&\hfil#\quad\cr';
$h4 = ' &\cr\tablerule';
$h5 = 'Tape & File & DSS & Date & Number of & Total Size\cr';
$h6 = 'Name & Number & VSN & File\_type & Records & (Byte)\cr\tablerule';
#$h7 = 'S26093 &  1 & A00068 & 8503.2D & 6756 & 221380608\cr';
#$h8 = 'S26093 & 22 & A00551 & 8612.2D & 6778 & 222101504\cr\tablerule';
$hend = '}\vskip 4pt}$$';
print HTML "$h1\n";
print HTML "$h2\n";
print HTML "$h3\n";
print HTML "$h4\n";
print HTML "$h5\n";
print HTML "$h6\n";
#
# loop through all VSNs
#
$a1 = '&';
$a2 = '\cr';
$a3 = '\cr\tablerule';
$a4 = '\omit';
$dssh = '/DSS/';
#
$nnf = 0;
$rgtt = 0;
$bgtt = 0;
open (INF, $file);
while (<INF>) {
  chop;
  if ( /p-A/ ) {
#   ($avsn, $rec, $dum1, $siz, $dum2, $ftyp) = split (" ");
#   ($dum3, $vsn) = split ("-", $avsn);
    ($avsn, @rec[$nnf], $dum1, @siz[$nnf], $dum2, @ftyp[$nnf]) = split (" ");
    ($dum3, @vsn[$nnf]) = split ("-", $avsn);
#   print HTML "$stape $a1 $nnf $a1 $vsn $a1 $ftyp $a1 $rec $a1 $siz $a2\n";
#   print "$stape $a1 $nnf $a1 $vsn $a1 $ftyp $a1 $rec $a1 $siz $a2\n";
#   $rgtt = $rgtt + $rec;
#   $bgtt = $bgtt + $siz;
    print MSSJOB "$dssh$vsn[$nnf]\n";
    $nnf++;
    }

  if ( /p-Y/ ) {
    ($avsn, @ftyp[$nnf], @rec[$nnf], $dum1, @siz[$nnf]) = split (" ");
    ($dum3, @vsn[$nnf]) = split ("-", $avsn);
#   print HTML "$stape $a1 $nnf $a1 $vsn $a1 $ftyp $a1 $rec $a1 $siz $a2\n";
#   print "$stape $a1 $nnf $a1 $vsn $a1 $ftyp $a1 $rec $a1 $siz $a2\n";
#   $rgtt = $rgtt + $rec;
#   $bgtt = $bgtt + $siz;
    print MSSJOB "$dssh$vsn[$nnf]\n";
    $nnf++;
    }

}
#
# finish the TeX table
#
for ($ii = 0; $ii < $nnf; $ii++) {
  $jj = $ii + 1;
  if ( $ii < ($nnf-1) ) {
    print HTML 
      "$stape $a1 $jj $a1 $vsn[$ii] $a1 $ftyp[$ii] $a1 $rec[$ii] $a1 $siz[$ii] $a2\n";
    print 
      "$stape $a1 $jj $a1 $vsn[$ii] $a1 $ftyp[$ii] $a1 $rec[$ii] $a1 $siz[$ii] $a2\n";
       }
  else {
    print HTML 
      "$stape $a1 $jj $a1 $vsn[$ii] $a1 $ftyp[$ii] $a1 $rec[$ii] $a1 $siz[$ii] $a3\n";
    print 
      "$stape $a1 $jj $a1 $vsn[$ii] $a1 $ftyp[$ii] $a1 $rec[$ii] $a1 $siz[$ii] $a3\n";
       }
  $rgtt = $rgtt + $rec[$ii];
  $bgtt = $bgtt + $siz[$ii];
}
$h9 = "Grand Total $a1 $a4 $a1 $a4 $a1 $a4 $a1 $rgtt $a1 $bgtt $a3";
print HTML "$h9\n";
print "$h9\n";
print HTML "$hend\n";
close (INF);
close (MSSJOB);
