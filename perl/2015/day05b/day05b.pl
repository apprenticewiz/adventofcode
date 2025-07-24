#!/usr/bin/env perl

use strict;
use warnings;

sub usage {
    my $progname = $0;
    print "usage: $progname <input file>\n";
    exit 1;
}

sub prop1 {
    my ($str) = @_;
    return ($str =~ /(..).*\1/);
}

sub prop2 {
    my ($str) = @_;
    return ($str =~ /(.).\1/);
}

sub process {
    my ($filename) = @_;
    my $counter = 0;

    open ( my $infile, '<', $filename ) or die "cannot open file: $filename: $!";
    while ( my $line = <$infile> ) {
        if ( prop1($line) && prop2($line) ) {
            $counter++;
        }
    }
    close($infile);

    return $counter;
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
