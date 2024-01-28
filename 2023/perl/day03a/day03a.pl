#!/usr/bin/env perl

use strict;
use warnings;

sub usage {
    print "usage: $0 <file>\n";
    exit 1;
}

sub build_numbers {
    my ($contents) = @_;

    my $row = 0;
    my $scanning_number = 0;
    my $number = "";
    my $current_pos = "(-1, -1)";
    my %number_locs;
    foreach my $line (split /\n/, $contents) {
        for my $col (0..(length $line) - 1) {
            my $ch = substr($line, $col, 1);
            if ( $scanning_number == 1 ) {
                if ( $ch =~ /\d/ ) {
                    $number .= $ch;
                } else {
                    $number_locs{$current_pos} = $number;
                    $number = "";
                    $scanning_number = 0;
                }
            } else {
                if ( $ch =~ /\d/ ) {
                    $number .= $ch;
                    $current_pos = "($row, $col)";
                    $scanning_number = 1;
                }
            }
        }
        if ( $scanning_number == 1 ) {
            $number_locs{$current_pos} = $number;
            $scanning_number = 0;
            $number = "";
        }
        $row++;
    }
    return %number_locs;
}

sub build_parts {
    my ($contents) = @_;

    my $row = 0;
    my %part_locs;
    foreach my $line (split /\n/, $contents) {
        for my $col (0..(length $line) - 1) {
            my $ch = substr($line, $col, 1);
            if ( !($ch =~ /\d/) && ($ch ne ".") ) {
                $part_locs{"($row, $col)"} = $ch;
            }
        }
        $row++;
    }
    return %part_locs;
}

sub check_parts {
    my ($number_locs_ref, $part_locs_ref) = @_;

    my %number_locs = %{$number_locs_ref};
    my %part_locs = %{$part_locs_ref};
    my $result = 0;
    my @neighbors = ([-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]);
    foreach my $number_loc ( keys %number_locs ) {
        $number_loc =~ /\((\d+), (\d+)\)/;
        my $number_row = int($1);
        my $number_col = int($2);
        my $number = $number_locs{$number_loc};
        my $number_col_first = $number_col;
        my $number_col_last = $number_col + length($number) - 1;
        my $found = 0;
        for my $num_col ($number_col_first..$number_col_last) {
            foreach my $neighbor_elem ( @neighbors ) {
                my @neighbor = @{$neighbor_elem};
                my $adj_row = $number_row + $neighbor[0];
                my $adj_col = $num_col + $neighbor[1];
                foreach my $part_loc ( keys %part_locs ) {
                    $part_loc =~ /\((\d+), (\d+)\)/;
                    my $part_row = int($1);
                    my $part_col = int($2);
                    if ( $part_row == $adj_row && $part_col == $adj_col ) {
                        $found = 1;
                        last;
                    }
                }
                last if ( $found == 1 );
            }
            last if ( $found == 1 );
        }
        if ( $found == 1 ) {
            $result += int($number);
        }
    }
    return $result;
}

sub process {
    my ($contents) = @_;

    my %number_locs = build_numbers($contents);
    my %part_locs = build_parts($contents);
    return check_parts(\%number_locs, \%part_locs);
}

sub main {
    if ( @ARGV < 1 ) {
        usage();
    }

    my $filename = $ARGV[0];

    open my $infile, '<', $filename or die "read of input file '$filename' failed.\n";
    my $contents = do { local $/; <$infile> };
    close $infile;

    my $result = process($contents);
    print "result = $result\n";
}

if ( $0 eq __FILE__ ) {
    main();
}
