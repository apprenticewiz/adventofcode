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
    my %positions;
    my $santa = [0, 0];
    my $position = join ',', @$santa;
    $positions{$position} = 1;

    open ( my $infile, '<', $filename ) or die "cannot open file: $filename: $!";
    while ( my $line = <$infile> ) {
        foreach my $ch ( split //, $line ) {
            if ( $ch eq '^' ) {
                $santa->[1] += 1;
            } elsif ( $ch eq 'v' ) {
                $santa->[1] -= 1;
            } elsif ( $ch eq '<' ) {
                $santa->[0] -= 1;
            } elsif ( $ch eq '>' ) {
                $santa->[0] += 1;
            }
            $position = join ',', @$santa;
            $positions{$position} = 1;
        }
    }
    close($infile);

    return scalar keys %positions;
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
