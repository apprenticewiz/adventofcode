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
        memLen <- 0
        i <- 2
        while ( i <= nchar(line) ) {
            if ( substr(line, i, i) == '\\' ) {
                if ( substr(line, i + 1, i + 1) %in% c('\\', '"') ) {
                    i <- i + 2
                } else if ( substr(line, i + 1, i + 1) == 'x' ) {
                    i <- i + 4
                } else {
                    i <- i + 1
                }
            } else {
                i <- i + 1
            }
            memLen <- memLen + 1
        }
        result <- result + codeLen - memLen
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
