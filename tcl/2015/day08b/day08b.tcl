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
        set encLen 0
        for { set i 0 } { $i < [string length $line] } { incr i } {
            if { [string equal [string index $line $i] "\\"] } {
                set encLen [expr {$encLen + 2}]
            } elseif { [string equal [string index $line $i] "\""] } {
                set encLen [expr {$encLen + 2}]
            } else {
                incr encLen
            }
        }
        set result [expr {$result + 2 + ($encLen - $codeLen)}]
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
