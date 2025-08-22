#!/usr/bin/env perl

use strict;
use warnings;

sub usage {
    my $progname = $0;
    print "usage: $progname <input file>\n";
    exit 1;
}

sub init_grid() {
    my @grid;

    for my $row (0..999) {
        for my $col (0..999) {
            $grid[$row][$col] = 0;
        }
    }
    return \@grid;
}

sub perform {
    my ($grid, $action, $r1, $c1, $r2, $c2) = @_;

    for my $row ($r1..$r2) {
        for my $col ($c1..$c2) {
            if ( $action eq 'turn on' ) {
                $grid->[$row][$col] += 1;
            } elsif ( $action eq 'turn off' ) {
                $grid->[$row][$col] = ($grid->[$row][$col] > 0) ? $grid->[$row][$col] - 1 : 0;
            } elsif ( $action eq 'toggle' ) {
                $grid->[$row][$col] += 2;
           }
        }
    }
}

sub sum {
    my ($grid) = @_;
    my $sum = 0;

    for my $row (0..999) {
        for my $col (0..999) {
            $sum += $grid->[$row][$col];
        }
    }
    return $sum;
}

sub process {
    my ($filename) = @_;
    my $grid = init_grid();

    open ( my $infile, '<', $filename ) or die "cannot open file: $filename: $!";
    while ( my $line = <$infile> ) {
        if ( $line =~ /(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/ ) {
            my ($command, $r1, $c1, $r2, $c2) = ($1, $2, $3, $4, $5);
            perform($grid, $command, $r1, $c1, $r2, $c2);
        } else {
            next;
        }
    }
    close($infile);

    return sum($grid);
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
