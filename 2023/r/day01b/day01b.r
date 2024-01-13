#!/usr/bin/env Rscript

prog_name <- "day01b.r"

usage <- function() {
    cat("usage:", prog_name, "<file>\n")
    quit(status=1)
}

process <- function(filename) {
    sum <- 0
    digit_strs <- list(
        "0" = 0,
        "1" = 1,
        "2" = 2,
        "3" = 3,
        "4" = 4,
        "5" = 5,
        "6" = 6,
        "7" = 7,
        "8" = 8,
        "9" = 9,
        "zero" = 0,
        "one" = 1,
        "two" = 2,
        "three" = 3,
        "four" = 4,
        "five" = 5,
        "six" = 6,
        "seven" = 7,
        "eight" = 8,
        "nine" = 9
    )
    infile <- file(filename, "r")
    while ( length(line <- readLines(infile, 1)) > 0 ) {
        min_index <- NULL
        max_index <- NULL
        left_digit <- 0
        right_digit <- 0
        for ( digit_str in names(digit_strs) ) {
            if ( grepl(digit_str, line) ) {
                left_index <- min(gregexpr(digit_str, line)[[1]])
                if ( is.null(min_index) || left_index < min_index ) {
                    min_index <- left_index
                    left_digit <- digit_strs[[digit_str]]
                }
                right_index <- max(gregexpr(digit_str, line)[[1]])
                if ( is.null(max_index) || right_index > max_index ) {
                    max_index <- right_index
                    right_digit <- digit_strs[[digit_str]]
                }
            }
        }
        sum <- sum + (left_digit * 10) + right_digit
    }
    sum
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
