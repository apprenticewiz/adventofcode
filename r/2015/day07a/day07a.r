#!/usr/bin/env Rscript

Operators <- list(ASSIGN = 1, NOT = 2, AND = 3, OR = 4, LSHIFT = 5, RSHIFT = 6)

usage <- function() {
    args <- commandArgs()
    progname <- sub("^--file=", "", args[grep("^--file=", args)])
    cat("usage:", progname, "<input file>\n")
    quit(status = 1)
}

process <- function(filename) {
    operations <- list()
    lines <- suppressWarnings(readLines(filename))
    for ( line in lines ) {
        p1 <- "^(\\d+|\\w+) -> (\\w+)$"
        p2 <- "NOT (\\d+|\\w+) -> (\\w+)"
        p3 <- "(\\d+|\\w+) (AND|OR) (\\d+|\\w+) -> (\\w+)"
        p4 <- "(\\d+|\\w+) (LSHIFT|RSHIFT) (\\d+) -> (\\w+)"
        if ( grepl(p1, line) ) {
            m = regexec(p1, line)
            caps = regmatches(line, m)[[1]]
            op = list(operator="ASSIGN", src=caps[2])
            operations[[caps[3]]] <- op
        } else if ( grepl(p2, line) ) {
            m = regexec(p2, line)
            caps = regmatches(line, m)[[1]]
            op = list(operator="NOT", src=caps[2])
            operations[[caps[3]]] <- op
        } else if ( grepl(p3, line) ) {
            m = regexec(p3, line)
            caps = regmatches(line, m)[[1]]
            op = list(operator=caps[3], src1=caps[2], src2=caps[4])
            operations[[caps[5]]] <- op
        } else if ( grepl(p4, line) ) {
            m = regexec(p4, line)
            caps = regmatches(line, m)[[1]]
            op = list(operator=caps[3], src=caps[2], amt=as.integer(caps[4]))
            operations[[caps[5]]] <- op
        }
    }
    return(evaluate(operations, "a"))
}

cache <- list()

evaluate <- function(operations, expr) {
    if ( grepl("^\\d+$", expr) ) {
        return(as.integer(expr))
    } else if ( !is.null(cache[[expr]]) ) {
        return(cache[[expr]])
    } else {
        op <- operations[[expr]]
        if ( op$operator == "ASSIGN" ) {
            a <- evaluate(operations, op$src)
            r <- a
        } else if ( op$operator == "NOT" ) {
            a <- evaluate(operations, op$src)
            r <- bitwNot(a)
        } else if( op$operator == "AND" ) {
            a <- evaluate(operations, op$src1)
            b <- evaluate(operations, op$src2)
            r <- bitwAnd(a, b)
        } else if ( op$operator == "OR" ) {
            a <- evaluate(operations, op$src1)
            b <- evaluate(operations, op$src2)
            r <- bitwOr(a, b)
        } else if ( op$operator == "LSHIFT" ) {
            a <- evaluate(operations, op$src)
            r <- bitwShiftL(a, op$amt)
        } else if ( op$operator == "RSHIFT" ) {
            a <- evaluate(operations, op$src)
            r <- bitwShiftR(a, op$amt)
        }
        masked <- bitwAnd(r, 65535)
        cache[[expr]] <<- masked
        return(masked)
    }
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
