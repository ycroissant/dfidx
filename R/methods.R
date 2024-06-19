#' Methods for dfidx
#'
#' A `dfidx` is a `data.frame` with a "sticky" data.frame column
#' which contains the indexes. Specific methods of functions that
#' extract lines and/or columns of a `data.frame` are provided.
#'
#' @name methods.dfidx
#' @param x,object a `dfidx` object
#' @param i the row index
#' @param j the column index
#' @param drop if `TRUE` a vector is returned if the result is a one
#'     column `data.frame`
#' @param y the name or the position of the series one wishes to
#'     extract
#' @param value the value for the replacement method
#' @param row.names,optional arguments of the generic `as.data.frame`
#'     method, not used
#' @param n the number of rows for the print method
#' @param ... further arguments
#' @export
#' @author Yves Croissant
#' @return `as.data.frame` and `mean` return a `data.frame`, `[[` and
#'     `$` a vector, `[` either a `dfidx` or a vector, `$<-`
#'     and `[[<-` modify the values of an existing column or create a
#'     new column of a `dfidx` object, `print` is called for its side
#'     effect
#' @importFrom pillar new_pillar_shaft_simple tbl_sum pillar_shaft
#' @importFrom vctrs new_rcrd field
#' @importFrom dplyr bind_cols
#' @examples
#' if (requireNamespace("AER")){
#' data("TravelMode", package = "AER")
#' TM <- dfidx(TravelMode)
#' # extract a series (returns as a xseries object)
#' TM$wait
#' # or
#' TM[["wait"]]
#' # extract a subset of series (returns as a dfidx object)
#' TM[c("wait", "income")]
#' # extract a subset of rows and columns
#' TM[TM$income > 30, c("wait", "income")]
#' # dfidx, idx and xseries have print methods as (like tibbles), a n
#' # argument
#' print(TM, n = 3)
#' print(idx(TM), n = 3)
#' print(TM$income, n = 3)
#' # a dfidx object can be coerced to a data.frame
#' head(as.data.frame(TM))
#' }
"[.dfidx" <- function(x, i, j, drop = TRUE){
    idx.pos <- idx_name(x)
    # add the idx column to the return data.frame if :
    # - j > 1
    # - j == 1 & drop = FALSE
#    if (! missing(j) && (length(j) > 1 | ! drop))
    if (! missing(j) && (! is.logical(j)) && (length(j) > 1 | ! drop))
        j <- union(j, ifelse(is.numeric(j), as.numeric(idx.pos), names(idx.pos)))
    class <- class(x)
    clseries <- attr(x, "clseries")
    idx <- idx(x)
    mdrop <- missing(drop)
    Narg <- nargs() - ! mdrop

    # use the data.frame method for the dfidx
    if (Narg < 3L){
        if (any(i < 0) & any(i >0)) stop("negative and positive indexes can't be mixed")
        if (any(i < 0)){
            i <- seq_len(length(x))[i]
        }
        i <- union(i, ifelse(is.numeric(i), as.numeric(idx.pos), names(idx.pos)))
        mydata <- `[.data.frame`(x, i)
    }
    else{
        mydata <- as.data.frame(x)[i, j, drop = drop]
    }
    # coerse the result to a clseries if it is a series or to a
    # dfidx
    if (is.null(dim(mydata))){
        structure(mydata,
                  idx = idx,
                  class = c(clseries, class(mydata))
                  )
    }
    else{
        structure(mydata,
                  idx = idx,
                  class = class)
    }
}

#' @rdname methods.dfidx
#' @export
as.data.frame.dfidx <- function(x, row.names = NULL, optional = FALSE, ...){
    dfidx_class <- sapply(strsplit(class(x), "_"),
                          function(z) z[1]) %in% "dfidx"
    class(x) <- class(x)[! dfidx_class]
    attr(x, "row.names") <- 1:nrow(x)
    attr(x, "clseries") <- NULL
    x
}
    
#' @rdname methods.dfidx
#' @export
print.dfidx <- function(x, ..., n = 10L){
    if (! inherits(x, "tbl_df")){
        idx <- idx(x)
        x <- as.data.frame(x)
        if (! inherits(x, "tbl_df")){
            if (n < nrow(x))
                cat(paste("~~~~~~~\n", "first", n, "observations out of", nrow(x), "\n~~~~~~~\n"))
            stopifnot(length(n) == 1L)
            n <- if (n < 0L) 
                     max(nrow(x) + n, 0L)
                 else min(n, nrow(x))
            x <- x[seq_len(n), , drop = FALSE]
            print(x, ...)
        }
        else print(x, ..., n = n)
        cat("\n")
        print(idx, ..., n = n)
    }
    else{
        class(x) <- c("tbl_dfidx", "tbl_df", "tbl", "data.frame")
        tbl2vctr <- function(x) vctrs::new_rcrd(unclass(x), class = "vecidx")
        
        x$idx <- tbl2vctr(x$idx)
        pos_idx <- match("idx", names(x))
        x <- bind_cols(x[pos_idx], x[- pos_idx])
        print(x, ..., n = n)
    }
}

#' @export
format.vecidx <- function(x, ...){
    .cls <- attr(x, "ids")
    ids <- match(c(1, 2), .cls)
    id1 <- field(x, ids[1])
    id2 <- field(x, ids[2])
    paste(id1, id2, sep = ":")
}

#' @export
pillar_shaft.vecidx <- function(x, ...){
    out <- format(x)
    pillar::new_pillar_shaft_simple(out, min_width = 5)
}
 
#' @export
tbl_sum.tbl_dfidx <- function(x, ...) {
    default_header <- NextMethod()
    nms <- attr(x$idx, "names")
    ids <- attr(x$idx, "ids")
    idp <- match(c(1, 2), ids)
    names(ids) <- nms
    cards <- sapply(nms, function(f) length(unique(field(x$idx, f))))
    cards <- cards[idp]
    
    .header <- c(default_header,
                 "Indexes" = paste(cards[1], " (", names(cards)[1], ") x ", cards[2], " (", names(cards)[2], ") ", sep = ""),
                 "Balanced" = ifelse(prod(cards) == nrow(x), "yes", "no"))
    if (length(idp) != length(ids)){
        nesting <- ids[- idp]
        nested <- names(cards[nesting])
        nesting <- names(nesting)
        nesting_structure <- paste(sapply(1:length(nested),
                                          function(i) paste(nested[i], " (", nesting[i], ")", sep = "")),
                                   collapse = ", ")
        .header <- c(.header, "Nesting" = nesting_structure)
    }
    .header
}

#' @rdname methods.dfidx
#' @importFrom utils head
#' @export
head.dfidx <- function(x, n = 10L, ...) print(x, n = min(nrow(x), n), ...)

#' @rdname methods.dfidx
#' @export
"[[.dfidx" <- function(x, y){
    clseries <- attr(x, "clseries")
    .idx <- idx(x)
    class(x) <- "data.frame"
    if (y %in% names(x)){
        series <- x[[y]]
        if (! "idx" %in% class(series))
            series <- structure(series, idx = .idx, class = c(clseries, class(series)))
    }
    else series <- NULL
    series
}  

"[[.dfidx" <- function(x, y){
    clseries <- attr(x, "clseries")
    .idx <- idx(x)
    class(x) <- "data.frame"
    if (is.character(y)){
        if ((y %in% names(x)) | (y %in% names(.idx))){
            if (y %in% names(x)) series <- x[[y]] else series <- .idx[[y]]
        }
        else series <- NULL
    }
    else series <- x[[y]]
    if (! is.null(series))
        if (! "idx" %in% class(series))
            series <- structure(series, idx = .idx, class = c(clseries, class(series)))
    series
}

#' @rdname methods.dfidx
#' @export
"$.dfidx" <- function(x,y){
  "[["(x, paste(as.name(y)))
}

#' @rdname methods.dfidx
#' @export
"$<-.dfidx" <- function(object, y, value){
  # object : le data.frame
  # y : la variable
  # value : la nouvelle valeur
  object[[y]] <- value
  object
}

#' @rdname methods.dfidx
#' @export
"[[<-.dfidx" <- function(object, y, value){
  if (class(value)[1] == "xseries"){
    class(value) <- class(value)[-1]
    attr(value, "idx") <- NULL
  }
  object <- "[[<-.data.frame"(object, y, value = value)
  object
}

#' @rdname methods.dfidx
#' @export
print.xseries <- function(x, ..., n = 10L){
    posxseries <- match("xseries", class(x))
    class(x) <- class(x)[-(1:posxseries)]
    idx <- attr(x, "idx")
    attr(x, "idx") <- NULL
    print(x[seq_len(min(length(x), n))])
    print(idx, n = n)
}


#' @rdname methods.dfidx
#' @export
print.idx <- function(x, ..., n = 10L){
    ids <- paste(attr(x, "ids"))
    cat("~~~ indexes ~~~~\n")
    print(as.data.frame(x)[seq_len(min(nrow(x), n)), ])
    cat("indexes: ", paste(ids, collapse = ", "), "\n")
}    

#' @rdname methods.dfidx
#' @method mean dfidx
#' @export
mean.dfidx <- function(x, ...){
    alt <- idx(x)[[idx_name(x, 2)]]
    x <- x[, - idx_name(x)]
    result <- data.frame(lapply(x,
                                function(x){
                                    if (is.numeric(x)) result <- as.numeric(tapply(x, alt, mean))
                                    else{
                                        if (is.logical(x)){
                                            z <- tapply(x, alt, sum)
                                            result <- z == max(z)
                                        }
                                        if(is.character(x)) x <- factor(x, levels = unique(x))
                                        if (is.factor(x)) result <- factor(names(which.max(table(x))), levels = levels(x))
                                    }
                                    result
                                }
                                )
                         )
    result
}
