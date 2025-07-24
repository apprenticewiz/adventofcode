#!/usr/bin/env perl

use strict;
use warnings;

use List::Util qw(min);

sub usage {
    my $progname = $0;
    print "usage: $progname <input file>\n";
    exit 1;
}

sub process {
    my ($filename) = @_;
    my $totalArea = 0;

    open ( my $infile, '<', $filename ) or die "cannot open file: $filename: $!";
    while ( my $line = <$infile> ) {
	    my ($l, $w, $h) = map { $_ + 0 } split(/x/, $line);
	    my $area1 = $l * $w;
	    my $area2 = $l * $h;
	    my $area3 = $w * $h;
	    my $surfaceArea = (2 * $area1) + (2 * $area2) + (2 * $area3);
	    my $minArea = min($area1, $area2, $area3);
	    $totalArea += $surfaceArea + $minArea;
    }
    close($infile);

    return $totalArea;
}

sub main {
    if ( @ARGV < 1 ) {
        usage();
    }
    my $filename = $ARGV[0];
    my $result = process($filename);
    print "result = $result\n";
}

main();
