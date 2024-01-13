#!/usr/bin/env Rscript

PROG_NAME <- "day02b.r"

usage <- function() {
    cat("usage:", PROG_NAME, "<file>\n")
    quit(status=1)
}

process <- function(filename) {
    result <- 0
    infile <- file(filename, "r")
    while ( length(line <- readLines(infile, 1)) > 0 ) {
        if ( !is.null(strsplit(line, ": ")[[1]]) ) {
            line_parts <- strsplit(line, ": ")[[1]]
            game_part <- line_parts[[1]]
            draws_part <- line_parts[[2]]
            if ( !is.null(strsplit(game_part, ' ')[[1]]) ) {
                game_parts <- strsplit(game_part, ' ')[[1]]
                game_num_str <- game_parts[[2]]
                game_num <- as.numeric(game_num_str)
                red_needed <- 0
                green_needed <- 0
                blue_needed <- 0
                for ( draw_str in strsplit(draws_part, "; ")[[1]] ) {
                    for ( color_amount in strsplit(draw_str, ", ")[[1]] ) {
                        color_amount_parts <- strsplit(color_amount, ' ')[[1]]
                        amount_str <- color_amount_parts[[1]]
                        color <- color_amount_parts[[2]]
                        amount <- as.numeric(amount_str)
                        switch (color,
                            "red" = {
                                if ( amount > red_needed ) {
                                    red_needed <- amount
                                }
                            },
                            "green" = {
                                if ( amount > green_needed ) {
                                    green_needed <- amount
                                }
                            },
                            "blue" = {
                                if ( amount > blue_needed ) {
                                    blue_needed <- amount
                                }
                            },
                            stop("unknown color")
                        )
                    }
                }
                result <- result + red_needed * green_needed * blue_needed
            }
        }
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
