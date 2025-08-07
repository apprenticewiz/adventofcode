#!/usr/bin/env Rscript

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

process <- function(filename) {
    floors <- 0
    pos <- 0
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        chars <- strsplit(line, "")[[1]]
        for ( ch in chars ) {
            pos <- pos + 1
            if ( ch == '(' ) {
                floors <- floors + 1
            } else if ( ch == ')' ) {
                floors <- floors - 1
            }
            if ( floors < 0 ) {
                return(pos);
            }
        }
    }
    return(0)
}

main <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    if ( length(args) < 1 ) {
        usage()
    }
    filename <- args[1]
    result <- process(filename)
    cat("result =", result, "\n")
}

main()
