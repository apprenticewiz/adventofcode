#!/usr/bin/env tclsh

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc process {filename} {
    set total_len 0
    set infile [open $filename r]
    while { [gets $infile line] >= 0 } {
        set dims [split $line "x"]
	set l [lindex $dims 0]
	set w [lindex $dims 1]
	set h [lindex $dims 2]
	set perim1 [expr {2 * ($l + $w)}]
	set perim2 [expr {2 * ($l + $h)}]
	set perim3 [expr {2 * ($w + $h)}]
	set present_len [expr {min($perim1, $perim2, $perim3)}]
	set bow_len [expr {$l * $w * $h}]
	incr total_len [expr {$present_len + $bow_len}]
    }
    close $infile
    return $total_len
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
