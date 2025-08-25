#!/usr/bin/env tclsh

set ROW_MAX 1000
set COL_MAX 1000

array set grid {}

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc perform {action r1 c1 r2 c2} {
    global COL_MAX grid
    for {set row $r1} {$row <= $r2} {incr row} {
        for {set col $c1} {$col <= $c2} {incr col} {
            set idx [expr {$row * $COL_MAX + $col}]
            set old_val $grid($idx)
            if { $action eq "turn on" } {
                set grid($idx) [expr {$old_val + 1}]
            } elseif { $action eq "turn off" } {
                set grid($idx) [expr {max(0, $old_val - 1)}]
            } elseif { $action eq "toggle" } {
                set grid($idx) [expr {$old_val + 2}]
            }
        }
    }
}

proc process {filename} {
    global ROW_MAX COL_MAX grid
    for {set row 0} {$row < $ROW_MAX} {incr row} {
        for {set col 0} {$col < $COL_MAX} {incr col} {
            set idx [expr {$row * $COL_MAX + $col}]
            set grid($idx) 0
        }
    }
    set infile [open $filename r]
    while { [gets $infile line] >= 0 } {
        if {[regexp {(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)} $line -> action r1 c1 r2 c2]} {
           perform $action $r1 $c1 $r2 $c2
        }
    }
    close $infile
    set total 0
    foreach idx [array names grid] {
       set total [expr {$total + $grid($idx)}]
    }
    return $total
}

proc main {} {
    if { [llength $::argv] < 1} {
        usage
    }
    set filename [lindex $::argv 0]
    set result [process $filename]
    puts "result = $result"
}

main
