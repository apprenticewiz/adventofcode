#!/usr/bin/env Rscript

PROG_NAME <- "day01a.r"

usage <- function() {
    cat("usage:", PROG_NAME, "<file>\n")
    quit(status=1)
}

process <- function(filename) {
    nums <- c()
    digits <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    infile <- file(filename, "r")
    while ( length(line <- readLines(infile, 1)) > 0 ) {
        min_index <- nchar(line) + 1
        left_digit <- "x"
        for ( digit in digits ) {
            left_pos <- min(gregexpr(digit, line)[[1]], na.rm=TRUE)
            if ( left_pos != -1 ) {
                if ( left_pos < min_index ) {
                    min_index <- left_pos
                        left_digit <- digit
                }
            }
        }
        max_index <- 0
        right_digit <- "x"
        for ( digit in digits ) {
            right_pos <- max(gregexpr(digit, line)[[1]], na.rm=TRUE)
            if ( right_pos != -1 ) {
                if ( right_pos > max_index ) {
                    max_index <- right_pos
                        right_digit <- digit
                }
            }
        }
        num_str <- paste(left_digit, right_digit, sep="")
        num <- as.integer(num_str)
        nums <- c(nums, num)
    }
    sum(nums)
}

main <- function() {
    args <- commandArgs(trailingOnly=TRUE)
    if ( length(args) < 1 ) {
        usage()
    }
    filename <- args[1]
    result <- process(filename)
    cat("result =", result, "\n")
}

if (!interactive()) {
    main()
}
