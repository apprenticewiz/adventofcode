#!/usr/bin/env perl

use strict;
use warnings;

use constant TOTAL_RED   => 12;
use constant TOTAL_GREEN => 13;
use constant TOTAL_BLUE  => 14;

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
        my $valid = 1;

        foreach my $game_set (split '; ', $sets_part) {
            foreach my $draws (split ', ', $game_set) {
                my ($amount_str, $color) = split ' ', $draws;
		my $amount = int($amount_str);

                if ($color eq 'red' && $amount > TOTAL_RED) {
                    $valid = 0;
                }
                if ($color eq 'green' && $amount > TOTAL_GREEN) {
                    $valid = 0;
                }
                if ($color eq 'blue' && $amount > TOTAL_BLUE) {
                    $valid = 0;
                }
            }
        }

        if ($valid) {
            $result += $game_id;
        }
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
