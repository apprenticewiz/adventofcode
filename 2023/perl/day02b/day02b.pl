#!/usr/bin/env perl

use strict;
use warnings;

sub usage {
    print "usage: $0 <file>\n";
    exit 1;
}

sub process {
    my ($contents) = @_;
    my $result = 0;

    foreach my $line (split /\n/, $contents) {
        my ($game_part, $sets_part) = (split ': ', $line);
        my $game_id = int((split ' ', $game_part)[1]);
        my $red_needed = 0;
        my $green_needed = 0;
        my $blue_needed = 0;

        foreach my $game_set (split '; ', $sets_part) {
            foreach my $draws (split ', ', $game_set) {
                my ($amount_str, $color) = split ' ', $draws;
                my $amount = int($amount_str);

                if ($color eq 'red' && $amount > $red_needed) {
                    $red_needed = $amount;
                }
                if ($color eq 'green' && $amount > $green_needed) {
                    $green_needed = $amount;
                }
                if ($color eq 'blue' && $amount > $blue_needed) {
                    $blue_needed = $amount;
                }
            }
        }

        $result += $red_needed * $green_needed * $blue_needed;
    }

    return $result;
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
