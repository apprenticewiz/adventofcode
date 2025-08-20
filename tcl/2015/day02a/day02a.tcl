#!/usr/bin/env tclsh

proc usage {} {
    set progname $::argv0
    puts stderr "usage: $progname <input file>"
    exit 1
}

proc process {filename} {
    set total_area 0
    set infile [open $filename r]
    while { [gets $infile line] >= 0 } {
        set dims [split $line "x"]
	set l [lindex $dims 0]
	set w [lindex $dims 1]
	set h [lindex $dims 2]
	set area1 [expr {$l * $w}]
	set area2 [expr {$l * $h}]
	set area3 [expr {$w * $h}]
	set surface_area [expr {(2 * $area1) + (2 * $area2) + (2 * $area3)}]
	set min_area [expr min($area1, $area2, $area3)]
	incr total_area [expr {$surface_area + $min_area}]
    }
    close $infile
    return $total_area
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
