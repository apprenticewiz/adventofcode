#!/usr/bin/env tclsh

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc process {filename} {
    set floor 0
    set infile [open $filename r]
    while { [gets $infile line] >= 0 } {
        foreach ch [split $line ""] {
            if { $ch eq "(" } {
                incr floor
            } elseif { $ch eq ")" } {
                incr floor -1
            }
        }
    }
    close $infile
    return $floor
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
