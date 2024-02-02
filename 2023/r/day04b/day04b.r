#!/usr/bin/env Rscript

PROG_NAME <- "day04b.r"

usage <- function() {
    cat("usage:", PROG_NAME, "<file>\n")
    quit(status=1)
}

process <- function(filename) {
    result <- 0
    instances <- list()
    infile <- file(filename, "r")
    while ( length(line <- readLines(infile, 1)) > 0 ) {
        if ( !is.null(strsplit(line, ":\\s+")[[1]]) ) {
            line_parts <- strsplit(line, ":\\s+")[[1]]
            card_str <- line_parts[1]
            card_num <- as.integer(strsplit(card_str, "\\s+")[[1]][2])
            if ( is.null(instances[card_num][[1]]) ) {
                instances[card_num] <- 0
            }
            rest <- line_parts[2]
            if ( !is.null(strsplit(rest, '\\s+\\|\\s+')[[1]]) ) {
                nums_parts <- strsplit(rest, "\\s+\\|\\s+")[[1]]
                winning_str <- nums_parts[1]
                winning_set <- c()
                for ( num_str in strsplit(winning_str, "\\s+")[[1]] ) {
                    winning_set <- c(winning_set, as.integer(num_str))
                }
                hand_str <- nums_parts[2]
                hand_set <- c()
                for ( num_str in strsplit(hand_str, "\\s+")[[1]] ) {
                    hand_set <- c(hand_set, as.integer(num_str))
                }
                intersection <- c()
                for ( num in winning_set ) {
                    if ( is.element(num, hand_set) ) {
                        intersection <- c(intersection, num)
                    }
                }
                i <- card_num + 1
                while ( i < card_num + length(intersection) + 1 ) {
                    copies <- 0
                    if ( !is.null(instances[i][[1]]) ) {
                        copies <- copies + instances[[i]]
                    }
                    copies <- copies + 1
                    if ( !is.null(instances[card_num][[1]]) )  {
                        copies <- copies + instances[[card_num]]
                    }
                    instances[i] <- copies
                    i <- i + 1
                }
                result <- result + 1
            }
        }
    }
    for ( i in 1:(length(instances)) ) {
        result <- result + instances[[i]]
    }
    result
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
