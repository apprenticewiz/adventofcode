#!/usr/bin/env Rscript

PROG_NAME <- "day02a.r"

TOTAL_RED <- 12
TOTAL_GREEN <- 13
TOTAL_BLUE <- 14

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
                valid <- TRUE
                for ( draw_str in strsplit(draws_part, "; ")[[1]] ) {
                    for ( color_amount in strsplit(draw_str, ", ")[[1]] ) {
                        color_amount_parts <- strsplit(color_amount, ' ')[[1]]
                        amount_str <- color_amount_parts[[1]]
                        color <- color_amount_parts[[2]]
                        amount <- as.numeric(amount_str)
                        switch (color,
                            "red" = {
                                if ( amount > TOTAL_RED ) {
                                    valid <- FALSE
                                }
                            },
                            "green" = {
                                if ( amount > TOTAL_GREEN ) {
                                    valid <- FALSE
                                }
                            },
                            "blue" = {
                                if ( amount > TOTAL_BLUE ) {
                                    valid <- FALSE
                                }
                            },
                            stop("unknown color")
                        )
                    }
                }
                if ( valid ) {
                    result <- result + game_num
                }
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
