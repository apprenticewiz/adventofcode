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
    my $totalLength = 0;

    open ( my $infile, '<', $filename ) or die "cannot open file: $filename: $!";
    while ( my $line = <$infile> ) {
	    my ($l, $w, $h) = map { $_ + 0 } split(/x/, $line);
	    my $perim1 = 2 * ($l + $w);
	    my $perim2 = 2 * ($l + $h);
	    my $perim3 = 2 * ($w + $h);
	    my $presentLength = min($perim1, $perim2, $perim3);
	    my $bowLength = $l * $w * $h;
	    $totalLength += $presentLength + $bowLength;
    }
    close($infile);

    return $totalLength;
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
