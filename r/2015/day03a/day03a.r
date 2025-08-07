#!/usr/bin/env Rscript

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

process <- function(filename) {
    positions <- list()
    santa <- c(0, 0)
    key <- paste(santa[1], santa[2], sep = ",")
    positions[[key]] <- TRUE
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        chars <- strsplit(line, "")[[1]]
        for ( ch in chars ) {
            if ( ch == '^' ) {
                santa[2] <- santa[2] + 1
            } else if ( ch == 'v' ) {
                santa[2] <- santa[2] - 1
            } else if ( ch == '<' ) {
                santa[1] <- santa[1] - 1
            } else if ( ch == '>' ) {
                santa[1] <- santa[1] + 1
            }
            key <- paste(santa[1], santa[2], sep = ",")
            positions[[key]] <- TRUE
        }
    }
    return(length(positions))
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
