#!/usr/bin/env Rscript

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

perform <- function(grid, action, upperLeft, lowerRight) {
    rows <- (upperLeft[1] + 1):(lowerRight[1] + 1)
    cols <- (upperLeft[2] + 1):(lowerRight[2] + 1)
    if ( action == "turn on" ) {
        grid[rows, cols] <- grid[rows, cols] + 1
    } else if ( action == "turn off" ) {
        grid[rows, cols] <- pmax(0, grid[rows, cols] - 1)
    } else if ( action == "toggle" ) {
        grid[rows, cols] <- grid[rows, cols] + 2
    }
    grid
}

process <- function(filename) {
    grid <- matrix(0, 1000, 1000)
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        re <- regexec("(turn on|turn off|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)", line)
        matches <- regmatches(line, re)[[1]]
        action <- matches[2]
        upperLeft <- as.integer(c(matches[3], matches[4]))
	lowerRight <- as.integer(c(matches[5], matches[6]))
	grid <- perform(grid, action, upperLeft, lowerRight)
    }
    return(sum(grid))
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
