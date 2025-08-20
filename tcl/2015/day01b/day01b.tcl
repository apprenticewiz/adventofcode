#!/usr/bin/env tclsh

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc process {filename} {
    set floor 0
    set pos 0
    set infile [open $filename r]
    while { [gets $infile line] >= 0 } {
        foreach ch [split $line ""] {
            incr pos
            if { $ch eq "(" } {
                incr floor
            } elseif { $ch eq ")" } {
                incr floor -1
            }
            if { $floor < 0 } {
                close $infile
                return $pos
            }
        }
    }
    close $infile
    return $0
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
