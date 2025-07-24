#!/usr/bin/env perl

use strict;
use warnings;

use Digest::MD5 qw(md5_hex);

sub usage {
    my $progname = $0;
    print "usage: $progname <key>\n";
    exit 1;
}

sub process {
    my ($key) = @_;
    my $n = 1;
    while ( 1 ) {
        my $tryKey = $key . $n;
        my $digest = md5_hex($tryKey);
        if ( $digest =~ /^000000/ ) {
            return $n;
        } else {
            $n++;
        }
    }
}

sub main {
    if ( @ARGV < 1 ) {
        usage();
    }
    my $key = $ARGV[0];
    my $result = process($key);
    print "result = $result\n";
}

main();
