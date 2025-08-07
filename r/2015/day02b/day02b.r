#!/usr/bin/env Rscript

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

process <- function(filename) {
    totalLen <- 0
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        dims <- as.integer(strsplit(line, "x")[[1]])
        l <- dims[1]
        w <- dims[2]
        h <- dims[3]
        perim1 <- 2 * (l + w)
        perim2 <- 2 * (l + h)
        perim3 <- 2 * (w + h)
        presentLen <- min(perim1, perim2, perim3)
        bowLen <- l * w * h
        totalLen <- totalLen + presentLen + bowLen
    }
    return(totalLen)
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
