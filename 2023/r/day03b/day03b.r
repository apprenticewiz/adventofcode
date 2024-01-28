#!/usr/bin/env Rscript

PROG_NAME <- "day03b.r"

usage <- function() {
    cat("usage:", PROG_NAME, "<file>\n")
    quit(status=1)
}

buildNumbers <- function(filename) {
    numLocs <- list()
    scanningNumber <- FALSE
    row <- 1
    number <- c()
    currentPos <- list(row = -1, col = -1)
    infile <- file(filename, "r")
    while ( length(line <- readLines(infile, 1)) > 0 ) {
        for ( col in 1:nchar(line) ) {
            ch <- substr(line, col, col)
            if ( scanningNumber ) {
                if ( grepl("[0-9]", ch) ) {
                    number <- c(number, ch)
                } else {
                    numVal <- paste(number, collapse = '')
                    numLocs <- c(numLocs, list(row=currentPos$row, col=currentPos$col, number=numVal))
                    scanningNumber <- FALSE
                    number <- c()
                }
            } else {
                if ( grepl("[0-9]", ch) ) {
                    number <- c(number, ch)
                    currentPos <- list(row = row, col = col)
                    scanningNumber <- TRUE
                }
            }
        }
        if ( scanningNumber ) {
            numVal <- paste(number, collapse = '')
            numLocs <- c(numLocs, list(row=currentPos$row, col=currentPos$col, number=numVal))
            scanningNumber <- FALSE
            number <- c()
        }
        row <- row + 1
    }
    close(infile)
    return(numLocs)
}

buildParts <- function(filename) {
    partLocs <- list()
    row <- 1
    infile <- file(filename, "r")
    while ( length(line <- readLines(infile, 1)) > 0 ) {
        for ( col in 1:nchar(line) ) {
            ch <- substr(line, col, col)
            if ( ch == '*' ) {
                partLocs <- c(partLocs, list(row=row, col=col, part=ch))
            }
        }
        row <- row + 1
    }
    close(infile)
    return(partLocs)
}

checkParts <- function(numLocs, partLocs) {
    result <- 0
    for ( partLocIdx in seq(1, length(partLocs), by=3) ) {
        adjCount <- 0
        prod <- 1
        partRow <- partLocs[[partLocIdx]]
        partCol <- partLocs[[partLocIdx + 1]]
        for ( numLocIdx in seq(1, length(numLocs), by=3) ) {
            found <- FALSE
            numRow <- numLocs[[numLocIdx]]
            numColFirst <- numLocs[[numLocIdx + 1]]
            numStr <- numLocs[[numLocIdx + 2]]
            numColLast <- numColFirst + nchar(numStr) - 1
            for ( deltaRow in -1:1 ) {
                adjRow <- partRow + deltaRow
                for ( deltaCol in -1:1 ) {
                    adjCol <- partCol + deltaCol
                    for ( numCol in numColFirst:numColLast ) {
                        if ( adjRow == numRow && adjCol == numCol ) {
                            adjCount <- adjCount + 1
                            prod <- prod * as.integer(numStr)
                            found <- TRUE
                            break
                        }
                    }
                    if ( found ) {
                        break
                    }
                }
                if ( found ) {
                    break
                }
            }
        }
        if ( adjCount == 2 ) {
            result <- result + prod
        }
    }
    return(result)
}

process <- function(filename) {
    numLocs <- buildNumbers(filename)
    partLocs <- buildParts(filename)
    return(checkParts(numLocs, partLocs))
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
