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
    my %operations = ();
    my %cache = ();

    open ( my $infile, '<', $filename ) or die "cannot open file: $filename: $!";
    while ( my $line = <$infile> ) {
        if ( $line =~ /^(\d+|\w+) -> (\w+)$/ ) {
            my ($src, $dest) = ($1, $2);
            $operations{$dest} = ["ASSIGN", $src];
        } elsif ( $line =~ /NOT (\d+|\w+) -> (\w+)/ ) {
            my ($src, $dest) = ($1, $2);
            $operations{$dest} = ["NOT", $src];
        } elsif ( $line =~ /(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)/ ) {
            my ($src1, $op, $src2, $dest) = ($1, $2, $3, $4);
            $operations{$dest} = [$op, $src1, $src2];
        } elsif ( $line =~ /(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)/ ) {
            my ($src, $op, $amt, $dest) = ($1, $2, $3, $4);
            $operations{$dest} = [$op, $src, $amt];
        } else {
            next;
        }
    }
    close($infile);

    my $a = evaluate(\%operations, \%cache, "a");
    $operations{"b"} = ["ASSIGN", $a];
    %cache = ();
    return evaluate(\%operations, \%cache, "a");
}

sub evaluate {
    my ($ops, $cache, $expr) = @_;

    if ( $expr =~ /\d+/ ) {
        return $expr;
    } elsif ( exists $cache->{$expr} ) {
        return $cache->{$expr};
    } else {
        my @op = @{ $ops->{$expr} };
        my $r = 0;
        my $operator = $op[0];
        if ( $operator eq "ASSIGN" ) {
            my $src = $op[1];
            my $a = evaluate($ops, $cache, $src);
            $r = $a;
        } elsif ( $operator eq "NOT" ) {
            my $src = $op[1];
            my $a = evaluate($ops, $cache, $src);
            $r = ~$a;
        } elsif ( $operator eq "AND" ) {
            my $src1 = $op[1];
            my $src2 = $op[2];
            my $a = evaluate($ops, $cache, $src1);
            my $b = evaluate($ops, $cache, $src2);
            $r = $a & $b;
        } elsif ( $operator eq "OR" ) {
            my $src1 = $op[1];
            my $src2 = $op[2];
            my $a = evaluate($ops, $cache, $src1);
            my $b = evaluate($ops, $cache, $src2);
            $r = $a | $b;
        } elsif ( $operator eq "LSHIFT" ) {
            my $src = $op[1];
            my $amt = $op[2];
            my $a = evaluate($ops, $cache, $src);
            $r = $a << $amt;
        } elsif ( $operator eq "RSHIFT" ) {
            my $src = $op[1];
            my $amt = $op[2];
            my $a = evaluate($ops, $cache, $src);
            $r = $a >> $amt;
        }
        my $masked = $r & 0xffff;
        $cache->{$expr} = $masked;
        return $masked;
    }
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
