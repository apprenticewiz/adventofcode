#!/usr/bin/env perl

use strict;
use warnings;

sub usage {
    my $progname = $0;
    print "usage: $progname <input file>\n";
    exit 1;
}

sub process {
    my ($filename) = @_;
    my $result = 0;

    open ( my $infile, '<', $filename ) or die "cannot open file: $filename: $!";
    while ( my $line = <$infile> ) {
        my $codeLen = length($line);
        my $encLen = 0;
        for my $i (0..(length($line) - 1)) {
            if ( substr($line, $i, 1) eq '\\' || substr($line, $i, 1) eq '"' ) {
                $encLen += 2;
            } else {
                $encLen += 1;
            }
        }
        $result += 2 + ($encLen - $codeLen);
    }
    close($infile);

    return $result;
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
