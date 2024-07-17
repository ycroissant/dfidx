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
#' @importFrom vctrs new_rcrd field vec_ptype_abbr
#' @importFrom dplyr bind_cols
#' @examples
#' mn <- dfidx(munnell)
#' # extract a series (returns as a xseries object)
#' mn$gsp
#' # or
#' mn[["gsp"]]
#' # extract a subset of series (returns as a dfidx object)
#' mn[c("gsp", "unemp")]
#' # extract a subset of rows and columns
#' mn[mn$unemp > 10, c("utilities", "water")]
#' # dfidx, idx and xseries have print methods as (like tibbles), a n
#' # argument
#' print(mn, n = 3)
#' print(idx(mn), n = 3)
#' print(mn$gsp, n = 3)
#' # a dfidx object can be coerced to a data.frame
#' head(as.data.frame(mn))
"[.dfidx" <- function(x, i, j, drop){
    idx.pos <- idx_name(x)
    mdrop <- missing(drop)
    if (mdrop) drop <- ifelse(inherits(x, "tbl_df"), FALSE, TRUE)
    clseries <- attr(x, "clseries")
    idx <- idx(x)
    Narg <- nargs() - ! mdrop
    .class <- class(x)
    class(x) <- setdiff(class(x), "dfidx")
    # then the data.frame or the tbl_df method we'll be used use the
    # data.frame method for the dfidx
    if (Narg < 3L){
        # adata[c("one", "two")], i is a vector of columns always
        # returns a data frame, even if only one column is selected
        # for data frames
        if (is.numeric(i)){
            if (any(i < 0) & any(i > 0)) stop("negative and positive indexes can't be mixed")
            if (any(i < 0)) i <- seq_len(length(x))[i]
            i <- union(i, as.numeric(idx.pos))
        }
        if (is.character(i)){
            if (any(! i %in% names(x))) stop("unknown column")
            i <- union(i, names(idx.pos))
        }
        if (is.logical(i)){
            if (length(i) != length(x)) stop("wrong length of the logical vector")
            i[as.numeric(idx.pos)] <- TRUE
        }
        mydata <- `[`(x, i)
    }
    else{
        # i selects rows, j columns
        # add the idx column to the returned data.frame if :
        # - j > 1
        # - j == 1 & drop = FALSE
        if (! missing(j)){
            if (is.numeric(j)){
                if (any(j < 0) & any(j > 0)) stop("negative and positive indexes can't be mixed")
                if (any(j < 0)) j <- seq_len(length(x))[j]
                if (length(j) > 1 | ! drop) j <- union(j, as.numeric(idx.pos))
            }
            if (is.logical(j)){
                if (length(j) != length(x)) stop("wrong length of the logical vector")
                if (length(j) > 1 | ! drop) j[as.numeric(idx.pos)] <- TRUE
            }
            if (is.character(j)){
                if (any(! j %in% names(x))) stop("unknown column")
                if (length(j) > 1 | ! drop) j <- union(j, names(idx.pos))
            }
        }
        mydata <- `[`(x, i, j, drop = drop)
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
        structure(mydata, class = .class)
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
        .nms_idx <- names(idx_name(x))
        .pos_idx <- as.numeric(idx_name(x))
        class(x) <- c("tbl_dfidx", "tbl_df", "tbl", "data.frame")
        tbl2vctr <- function(x) vctrs::new_rcrd(unclass(x), class = "vecidx")
#        x$idx <- tbl2vctr(x$idx)
#        pos_idx <- match("idx", names(x))
#        x <- bind_cols(x[pos_idx], x[- pos_idx])
        x[[.nms_idx]] <- tbl2vctr(x[[.nms_idx]])
#        x <- bind_cols(x[.pos_idx], x[- .pos_idx])
        x
        print(x, ...)
    }
}

#' @export
vec_ptype_abbr.vecidx <- function(x, ..., prefix_named, suffix_shape) "idx"

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
    pillar::new_pillar_shaft_simple(out, min_width = 8, shorten = "mid")
}

index_structure <- function(x){
    nms <- attr(x, "names")
    ids <- attr(x, "ids")
    idp <- match(c(1, 2), ids)
    names(ids) <- nms
    cards <- sapply(nms, function(f) length(unique(field(x, f))))
    cards <- cards[idp]
    .indexes <- paste(cards[1], " (", names(cards)[1], ") x ", cards[2], " (", names(cards)[2], ") ", sep = "")
    .balanced <- ifelse(prod(cards) == length(x), "yes", "no")
    result <- c(Index = .indexes, Balanced = .balanced)
    if (length(idp) != length(ids)){
        nesting <- ids[- idp]
        nested <- names(cards[nesting])
        nesting <- names(nesting)
        nesting_structure <- paste(sapply(1:length(nested),
                                          function(i) paste(nested[i], " (", nesting[i], ")", sep = "")),
                                   collapse = ", ")
        .nesting <- nesting_structure
        result <- c(result, Nesting = .nesting)
    }
    result
}


#' @export
tbl_sum.tbl_dfidx <- function(x, ...) {
    default_header <- NextMethod()
    .idx <- names(which(sapply(x, function(aserie) inherits(aserie, "vecidx"))))
    c(default_header, index_structure(x[[.idx]]))
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
#' @importFrom glue glue
print.xseries <- function(x, ..., n = 10L){
    # just remove xseries from the class ?
#    posxseries <- match("xseries", class(x))
#    class(x) <- class(x)[-(1:posxseries)]
    class(x) <- setdiff(class(x), "xseries")
    idx <- attr(x, "idx")
    attr(x, "idx") <- NULL
    if (inherits(idx, "tbl_df")) cat(glue("# Index: ", index_structure(idx)["Index"]), "\n")
    print(x[seq_len(min(length(x), n))])
    if (! inherits(idx, "tbl_df")) print(idx, n = n)
}


#' @rdname methods.dfidx
#' @export
print.idx <- function(x, ..., n = 10L){
    if (! inherits(x, "tbl_df")){
        ids <- paste(attr(x, "ids"))
        cat("~~~ indexes ~~~~\n")
        print(as.data.frame(x)[seq_len(min(nrow(x), n)), ])
        cat("indexes: ", paste(ids, collapse = ", "), "\n")
    }
    else{
        class(x) <- setdiff(class(x), "idx")
        print(x, ...)
    }
}    

#' @rdname methods.dfidx
#' @method mean dfidx
#' @export
mean.dfidx <- function(x, ...){
    alt <- idx(x)[[idx_name(x, 2)]]
    x <- as.data.frame(x)[, - idx_name(x)] #!!!
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
