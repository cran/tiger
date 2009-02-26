LCS <- function(a,b){
    stopifnot(is.character(a),is.character(b))
    if(any(is.na(c(a,b)))){
       out <- list(a = a, b = b, LLCS = NA, LCS = NA, QSI = NA, va = NA, vb = NA)
    } else {
       out <- .Call("lcs", as.character(a), as.character(b), max(nchar(c(a,b))), PACKAGE="tiger")
    }
    invisible(out)
}


