#!/usr/bin/env Rscript

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

process <- function(filename) {
    result <- 0
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        codeLen <- nchar(line)
        encLen <- 0
        for ( i in 1:nchar(line) ) {
            if ( substr(line, i, i) %in% c('\\', '"') ) {
                encLen <- encLen + 2
            } else {
                encLen <- encLen + 1
            }
        }
        result <- result + 2 + (encLen - codeLen)
    }
    return(result)
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
