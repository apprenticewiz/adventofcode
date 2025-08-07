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
    roboSanta <- c(0, 0)
    key <- paste(santa[1], santa[2], sep = ",")
    positions[[key]] <- TRUE
    santaMove <- TRUE
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        chars <- strsplit(line, "")[[1]]
        for ( ch in chars ) {
            if ( santaMove ) {
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
            } else {
                if ( ch == '^' ) {
                    roboSanta[2] <- roboSanta[2] + 1
                } else if ( ch == 'v' ) {
                    roboSanta[2] <- roboSanta[2] - 1
                } else if ( ch == '<' ) {
                    roboSanta[1] <- roboSanta[1] - 1
                } else if ( ch == '>' ) {
                    roboSanta[1] <- roboSanta[1] + 1
                }
                key <- paste(roboSanta[1], roboSanta[2], sep = ",")
            }
            positions[[key]] <- TRUE
	    santaMove <- !santaMove
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
