#!/usr/bin/env tclsh

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc process {filename} {
    set result 0
    set infile [open $filename r]
    while { [gets $infile line] >= 0 } {
        set codeLen [string length $line]
        set memLen 0
        set i 1
        while { $i < ([string length $line] - 1) } {
            if { [string equal [string index $line $i] "\\"] } {
                set ch [string index $line [expr {$i + 1}]]
                if { [string equal $ch "\\"] } {
                    set i [expr {$i + 2}]
                } elseif { [string equal $ch "\""] } {
                    set i [expr {$i + 2}]
                } elseif { [string equal $ch "x"] } {
                    set i [expr {$i + 4}]
                } else {
                    incr i
                }
            } else {
                incr i
            }
            incr memLen
        }
        set result [expr {$result + ($codeLen - $memLen)}]
    }
    close $infile
    return $result
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
