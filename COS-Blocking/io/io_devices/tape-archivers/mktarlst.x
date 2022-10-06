#! /bin/csh
cd ~/DJ_DLT
set tgdr = '/dev/rmt/3n'
set outf = 'B00042.tarlst'
set mxf = 11
#set blks = 512
#set blks = 64
set blks = 20
#----------------------------------
if ( ! -r $outf ) then
   touch $outf
endif
set sdate = `date`
echo $sdate
#mt -f $tgdr rew
@ nn=1
while ( $nn <= $mxf )
echo '..start file '$nn >> $outf
tar tvbf $blks $tgdr >> $outf
echo '..done  '$nn >> $outf
echo '..done  '$nn
@ nn++
end
set edate = `date`
echo $edate
echo '....starting '$sdate'     ....ending '$edate
exit
