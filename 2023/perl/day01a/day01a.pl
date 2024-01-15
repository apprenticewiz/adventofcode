#!/usr/bin/env perl

use strict;
use warnings;

sub usage {
    print "usage: $0 <file>\n";
    exit 1;
}

sub process {
    my ($contents) = @_;
    my @lines = split /\n/, $contents;
    my $result = 0;
    my @digits = (0..9);

    foreach my $line (@lines) {
        my %left_indices;
        my %right_indices;

        foreach my $digit (@digits) {
            if (index($line, $digit) != -1) {
                $left_indices{$digit} = index($line, $digit);
                $right_indices{$digit} = rindex($line, $digit);
            }
        }

        my $left_digit = (sort { $left_indices{$a} <=> $left_indices{$b} } keys %left_indices)[0];
        my $right_digit = (sort { $right_indices{$b} <=> $right_indices{$a} } keys %right_indices)[0];

        $result += int($left_digit . $right_digit);
    }

    return $result
}

sub main {
    if (@ARGV < 1) {
        usage();
    }

    my $filename = $ARGV[0];

    open my $infile, '<', $filename or die "read of input file '$filename' failed.\n";
    my $contents = do { local $/; <$infile> };
    close $infile;

    my $result = process($contents);
    print "result = $result\n";
}

if ($0 eq __FILE__) {
    main();
}
