#!/usr/bin/env tclsh

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc prop1 {str} {
    return [regexp {(..).*\1} $str]
}

proc prop2 {str} {
    return [regexp {(.).\1} $str]
}

proc process {filename} {
    set count 0
    set infile [open $filename r]
    while { [gets $infile line] >= 0 } {
        if { [prop1 $line] && [prop2 $line] } {
            incr count
        }
    }
    close $infile
    return $count
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
