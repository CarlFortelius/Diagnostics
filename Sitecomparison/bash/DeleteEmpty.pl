#!/usr/bin/perl
#
# Usage: DeleteEmpty.pl  [files]

use strict;
use warnings;

if (!@ARGV) {
   @ARGV = <STDIN>;
   chomp(@ARGV);
}


foreach my $file (@ARGV) {
   if (-z $file){
   print "Deleting zero size file: $file \n";
   unlink $file;
   }
}

exit(0);
