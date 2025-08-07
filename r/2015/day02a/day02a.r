#!/usr/bin/env Rscript

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

process <- function(filename) {
    totalArea <- 0
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        dims <- as.integer(strsplit(line, "x")[[1]])
        l <- dims[1]
        w <- dims[2]
        h <- dims[3]
        area1 <- l * w
        area2 <- l * h
        area3 <- w * h
        surfaceArea <- 2 * area1 + 2 * area2 + 2 * area3
        minArea <- min(area1, area2, area3)
        totalArea <- totalArea + surfaceArea + minArea
    }
    return(totalArea)
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
