#!/usr/bin/env perl

use strict;
use warnings;

sub usage {
    print "usage: $0 <file>\n";
    exit 1;
}

sub process {
    my ($contents) = @_;
    my $total = 0;
    my @lines = split(/\n/, $contents);
    foreach my $line (@lines) {
        my (undef, $rest) = split(':', $line, 2);
        my ($winning_str, $hand_str) = split(/\|/, $rest, 2);
        my %winning = map { $_ => 1 } split(' ', $winning_str);
        my %hand = map { $_ => 1 } split(' ', $hand_str);
        my $count = scalar(grep { exists $winning{$_} } keys %hand);
        $total += 2**($count - 1) if $count > 0;
    }
    return $total;
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
