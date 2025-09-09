#!/usr/bin/env tclsh

set operations [dict create]
set cache [dict create]

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc evaluate {exp} {
    global operations cache
    if {[regexp {^\d+$} $exp]} {
        return $exp
    } elseif {[dict exists $cache $exp]} {
        return [dict get $cache $exp]
    } else {
        set op [dict get $operations $exp]
        set r 0
        switch [lindex $op 0] {
            "ASSIGN" {
                set r [evaluate [lindex $op 1]]
            }
            "NOT" {
                set a [evaluate [lindex $op 1]]
                set r [expr {~$a}]
            }
            "AND" {
                set a [evaluate [lindex $op 1]]
                set b [evaluate [lindex $op 2]]
                set r [expr {$a & $b}]
            }
            "OR" {
                set a [evaluate [lindex $op 1]]
                set b [evaluate [lindex $op 2]]
                set r [expr {$a | $b}]
            }
            "LSHIFT" {
                set a [evaluate [lindex $op 1]]
                set r [expr {$a << [lindex $op 2]}]
            }
            "RSHIFT" {
                set a [evaluate [lindex $op 1]]
                set r [expr {$a >> [lindex $op 2]}]
            }
        }
        set masked [expr {$r & 65535}]
        dict set cache $exp $masked
        return $masked
    }
}

proc process {filename} {
    global operations cache
    set infile [open $filename r]
    while { [gets $infile line] >= 0 } {
        if {[regexp {^(\d+|\w+) -> (\w+)$} $line -> src dest]} {
            dict set operations $dest [list "ASSIGN" $src]
        } elseif {[regexp {NOT (\d+|\w+) -> (\w+)} $line -> src dest]} {
            dict set operations $dest [list "NOT" $src]
        } elseif {[regexp {(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)} $line -> src1 op src2 dest]} {
            dict set operations $dest [list $op $src1 $src2]
        } elseif {[regexp {(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)} $line -> src op amt dest]} {
            dict set operations $dest [list $op $src $amt]
        }
    }
    close $infile
    return [evaluate "a"]
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
