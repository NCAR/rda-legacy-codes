#! /usr/local/bin/perl
#
$file = $ARGV[0];
#----------------------------------------------------
open (INF, $file);
while (<INF>) {
  chop;
  if ( ! /sanl/ ) {
  print "$_\n";
  }
}
close (INF);
