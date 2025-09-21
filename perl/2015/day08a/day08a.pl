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
        my $memLen = 0;
        my $i = 1;
        while ( $i < length($line) - 1 ) {
            if ( substr($line, $i, 1) eq '\\' ) {
                if ( substr($line, $i + 1, 1) eq '\\' || substr($line, $i + 1, 1) eq '"' ) {
                    $i += 2;
                } elsif ( substr($line, $i + 1, 1) eq 'x' ) {
                    $i += 4;
                } else {
                    $i += 1;
                }
            } else {
                $i += 1;
            }
            $memLen += 1;
        }
        $result += $codeLen - $memLen;
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
