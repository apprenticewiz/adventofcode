#!/usr/bin/env tclsh

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc process {filename} {
    set total_area 0
    set infile [open $filename r]
    set santa_x 0
    set santa_y 0
    set robo_santa_x 0
    set robo_santa_y 0
    set santa_move 1
    array set positions {}
    set key "($santa_x, $santa_y)"
    set positions($key) 1
    while { [gets $infile line] >= 0 } {
        foreach ch [split $line ""] {
            if { $santa_move } {
                if { $ch eq "^" } {
                    incr santa_y
                } elseif { $ch eq "v" } {
                    incr santa_y -1
                } elseif { $ch eq "<" } {
                    incr santa_x -1
                } elseif { $ch eq ">" } {
                    incr santa_x
                }
                set key "($santa_x, $santa_y)"
            } else {
                if { $ch eq "^" } {
                    incr robo_santa_y
                } elseif { $ch eq "v" } {
                    incr robo_santa_y -1
                } elseif { $ch eq "<" } {
                    incr robo_santa_x -1
                } elseif { $ch eq ">" } {
                    incr robo_santa_x
                }
                set key "($robo_santa_x, $robo_santa_y)"
            }
            set positions($key) 1
            set santa_move [expr {! $santa_move}]
        }
    }
    close $infile
    return [llength [array names positions]]
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
