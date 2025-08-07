#!/usr/bin/env Rscript

library(digest)

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

process <- function(key) {
    n <- 0
    repeat {
        tryKey <- paste0(key, n)
        hexDigest <- digest(tryKey, algo = "md5", serialize = FALSE)
        if ( startsWith(hexDigest, "000000") ) {
            return(n)
        }
        n <- n + 1
    }
}

main <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    if ( length(args) < 1 ) {
        usage()
    }
    key <- args[1]
    result <- process(key)
    cat("result =", result, "\n")
}

main()
