#! /usr/local/bin/perl
#
$file = $ARGV[0];
#----------------------------------------------------
$h1= ' ';
$h2= 'LIST OF FILES ON DLT Bxxxxx.x:';
$h3= '        This is a unix tar tape with multiple tar files.';
$h4= '        Blocksize used: 32768 bytes or 64 blocks.';
$h5= '        Please use "no rewind" device name.';
$h6= '------------------------------------------------------------------------';
$h7= '   File  MSS                                                Size (Byte)';
$h8= '     No  Name';
$h9= '------------------------------------------------------------------------';
print "$h1\n";
print "$h2\n";
print "$h3\n";
print "$h4\n";
print "$h5\n";
print "$h6\n";
print "$h7\n";
print "$h8\n";
print "$h9\n";
#
# loop through all lines
#echo $aline | awk '{print "  "$1"	"$6"		"$10}'
#
$totby = 0;
#format STDOUT =
#@>>>>>>  @<<<<<<<<<<<<<<<<<<<<<<<<< @>>>>>>>>>>>>>>>
#$nn      $fnam                      $siz
#.
format STDOUT =
@>>>>>>  @<<<<<<< @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @>>>>>>>>>>>>>>>
$nn      $avsn    $fnam                                $siz
.
format TOT =
@>>>>>>                                                @>>>>>>>>>>>>>>>
"TOTAL"                                                $totby
.
$nn = 1;
open (INF, $file);
while (<INF>) {
  chop;
# ($dum1,$dum2,$dum3,$dum4,$siz,$dum6,$dum7,$dum8,$fnam) = split (" ");
  ($dum1,$avsn,$dum3,$dum4,$siz,$dum6,$fnam,$dum8,$dum9) = split (" ");
# print "  $nn   $avsn  $fnam		$siz\n";
  write;
  $totby = $totby + $siz;
  $nn++;
}
print "$h9\n";
$~ = "TOT";
write;
print "$h9\n";
close (INF);
