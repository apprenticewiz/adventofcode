#!/usr/bin/env tclsh

package require md5

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <key>"
    exit 1
}

proc process {key} {
    set n 1
    while { 1 } {
        set try_key "$key$n"
	set digest [string tolower [::md5::md5 -hex $try_key]]
        if { [string equal [string range $digest 0 4] "00000"] } {
            break
        }
        incr n
    }
    return $n
}

proc main {} {
    if { [llength $::argv] < 1} {
        usage
    }
    set key [lindex $::argv 0]
    set result [process $key]
    puts "result = $result"
}

main
