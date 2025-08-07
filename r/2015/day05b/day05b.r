#!/usr/bin/env Rscript

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

prop1 <- function(str) {
    return(grepl("(..).*\\1", str, perl = TRUE));
}

prop2 <- function(str) {
    return(grepl("(.).\\1", str));
}

process <- function(filename) {
    count <- 0
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        if ( prop1(line) && prop2(line) ) {
            count <- count + 1
        }
    }
    return(count)
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
