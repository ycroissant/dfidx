#' Methods for dfidx
#'
#' A `dfidx` object is a data frame with a "sticky" data frame column
#' which contains the indexes. Specific methods of functions that
#' extract lines and/or columns of a data frame are provided : `[`,
#' `[[`, `$`,`[<-`, `[[<-` and `$<-`. Moreover, methods are provided
#' for `base::transform` and `base::subset` in order to easily
#' generate new variables and select some rows and columns of a
#' `dfidx` oject. An `organize` function is also provided to sort a
#' `dfidx` object using one or several series.
#'
#' @name methods.dfidx
#' @param x,object,_data a `dfidx` object
#' @param i the row index (or the column index if `j` is not used)
#' @param j the column index
#' @param drop if `TRUE` a vector is returned if the result is a one
#'     column `data.frame`
#' @param y the name or the position of the series one wishes to
#'     extract
#' @param value the value for the replacement method
#' @param row.names,optional arguments of the generic `as.data.frame`
#'     method, not used
#' @param n the number of rows for the print method
#' @param subset,select see `base::subset`
#' @param drop.unused.levels passed to `dfidx::dfidx`
#' @param ... further arguments
#' @export
#' @author Yves Croissant
#' @return `as.data.frame` and `mean` return a `data.frame`, `[[` and
#'     `$` a vector, `[` either a `dfidx` or a vector, `$<-` and
#'     `[[<-` modify the values of an existing column or create a new
#'     column of a `dfidx` object. `transform`, `subset` and
#'     `organize` return a `dfidx` object. `print` is called for its
#'     side effect.
#' @examples
#' mn <- dfidx(munnell)
#' # extract a series (returns as a xseries object)
#' mn$gsp
#' # or
#' mn[["gsp"]]
#' # extract a subset of series (returned as a dfidx object)
#' mn[c("gsp", "unemp")]
#' # extract a subset of rows and columns
#' mn[mn$unemp > 10, c("utilities", "water")]
#' # dfidx, idx and xseries have print methods as (like tibbles), a n
#' # argument
#' print(mn, n = 3)
#' print(idx(mn), n = 3)
#' print(mn$gsp, n = 3)
#' # a dfidx object can be coerced to a data.frame
#' as.data.frame(mn)
#' # transform, subset and organize are usefull methods/function to
#' # create new series, select a subset of lines and/or columns and to
#' # sort the `dfidx` object using one or several series
#' transform(mn, gsp70 = ifelse(year == 1970, gsp, 0))
#' subset(mn, gsp > 200000, select = c("gsp", "unemp"))
#' subset(mn, 1:20, select = c("gsp", "unemp"))
#' organize(mn, year, unemp)
"[.dfidx" <- function(x, i, j, drop){
    idx.pos <- unname(idx_name(x))
    idx.nm <- names(idx_name(x))
    .first <- idx.pos == 1L
    mdrop <- missing(drop)
    if (mdrop) drop <- ifelse(inherits(x, "tbl_df"), FALSE, TRUE)
    clseries <- attr(x, "clseries")
    idx <- idx(x)
    Narg <- nargs() - ! mdrop
    .class <- class(x)
    .clseries <- attr(x, "clseries")
    class(x) <- setdiff(class(x), "dfidx")
    # then the data.frame or the tbl_df method we'll be used use the
    # data.frame method for the dfidx
    if (Narg < 3L){
        # adata[c("one", "two")], i is a vector of columns always
        # returns a data frame, even if only one column is selected
        # for data frames
        if (is.logical(i)){
            if (length(i) != length(x)) stop("wrong length of the logical vector")
            i[idx.pos] <- TRUE
            i <- which(i)
            # the numeric i is then treated by the following chunk
        }
        if (is.numeric(i)){
            if (any(i < 0) & any(i > 0)) stop("negative and positive indexes can't be mixed")
            if (any(i < 0)) i <- seq_len(length(x))[i]
            i <- setdiff(i, idx.pos)
            if (.first) i <- union(idx.pos, i) else i <- union(i, idx.pos)
        }
        if (is.character(i)){
            i <- setdiff(i, idx.nm)
            if (any(! i %in% names(x))) stop("unknown column")
            if (.first) i <- union(idx.nm, i) else i <- union(i, idx.nm)
        }
        mydata <- `[`(x, i)
    }
    else{
        # i selects rows, j columns
        # add the idx column to the returned data.frame if :
        # - j > 1
        # - j == 1 & drop = FALSE
        if (! missing(j)){
            if (is.logical(j)){
                if (length(j) != length(x)) stop("wrong length of the logical vector")
                if (length(j) > 1 | ! drop) j[idx.pos] <- TRUE
                j <- which(j)
            }
            if (is.numeric(j)){
                if (any(j < 0) & any(j > 0)) stop("negative and positive indexes can't be mixed")
                if (any(j < 0)) j <- seq_len(length(x))[j]
                j <- setdiff(j, idx.pos)
                if (length(j) > 1 | ! drop){
                    if (.first) j <- union(idx.pos, j) else j <- union(j, idx.pos)
                }
            }
            if (is.character(j)){
                if (any(! j %in% names(x))) stop("unknown column")
                j <- setdiff(j, idx.nm)
                if (length(j) > 1 | ! drop){
                    if (.first) j <- union(idx.nm, j) else j <- union(j, idx.nm)
                }
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
        rownames(mydata) <- NULL
        rownames(mydata[[idx.nm]]) <- NULL
        structure(mydata, class = .class, clseries = .clseries)
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
print.dfidx <- function(x, ..., n = NULL){
    if (is.null(n)) n <- options()$dfidx.print_n
    idx <- as.data.frame(idx(x))
    x <- as.data.frame(x)
    if (n < nrow(x))
        cat(paste("~~~~~~~\n", "first", n, "observations out of", nrow(x), "\n~~~~~~~\n"))
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) 
             max(nrow(x) + n, 0L)
         else min(n, nrow(x))
    x <- x[seq_len(n), , drop = FALSE]
    print(x, ...)
    cat("\n")
#    print(idx, ..., n = n)
    ids <- paste(attr(idx, "ids"))
    cat("~~~ indexes ~~~~\n")
    idx <- idx[seq_len(n), ]
    print(idx, ...)
    cat("indexes: ", paste(ids, collapse = ", "), "\n")
}

#' @rdname methods.dfidx
#' @importFrom utils head
#' @export
head.dfidx <- function(x, n = NULL, ...){
    if (is.null(n)) n <- options()$dfidx.print_n
    print(x, n = min(nrow(x), n), ...)
}

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
print.xseries <- function(x, ..., n = NULL){
    # just remove xseries from the class ?
#    posxseries <- match("xseries", class(x))
#    class(x) <- class(x)[-(1:posxseries)]
    if (is.null(n)) n <- options()$dfidx.print_n
    index_structure <- function(x){
        nms <- attr(x, "names")
        ids <- attr(x, "ids")
        idp <- match(c(1, 2), ids)
        names(ids) <- nms
#        cards <- sapply(nms, function(f) length(unique(field(x, f))))
        cards <- sapply(nms, function(f) length(unique(x[[f]])))
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
    class(x) <- setdiff(class(x), "xseries")
    idx <- attr(x, "idx")
    attr(x, "idx") <- NULL
    if (inherits(idx, "tbl_df")) cat(paste("# Index: ", index_structure(idx)["Index"], "\n", sep = "\n"))
    print(x[seq_len(min(length(x), n))])
    if (! inherits(idx, "tbl_df")) print(idx, n = n)
}

#' @rdname methods.dfidx
#' @export
print.idx <- function(x, ..., n = NULL){
    if (is.null(n)) n <- options()$dfidx.print_n
    if (! inherits(x, "tbl_df")){
        if (is.null(n)) n <- 10L
        ids <- paste(attr(x, "ids"))
        print(as.data.frame(x)[seq_len(min(nrow(x), n)), ])
        cat("indexes: ", paste(ids, collapse = ", "), "\n")
   }
   else{
       class(x) <- setdiff(class(x), "idx")
       print(x, ..., n = n)
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
                                    if (is.numeric(x)) result <- as.numeric(tapply(x, alt, mean, na.rm = TRUE))
                                    else{
                                        if (is.logical(x)){
                                            z <- tapply(x, alt, sum, na.rm = TRUE)
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

#' @rdname methods.dfidx
#' @method transform dfidx
#' @export
transform.dfidx <- function(`_data`, ...){
    dta <- `_data`
    attrs <- attributes(dta)
    dta <- unfold_idx(dta)
    .idx_vector <- attr(dta, "idx_vector")
    .idx_name <- attr(dta, "idx_name")
    dta <- transform(dta, ...)
    attr(dta, "idx_vector") <- .idx_vector
    attr(dta, "idx_name") <- .idx_name
    dta <- fold_idx(dta)
    attrs$names <- names(dta)
    attributes(dta) <- attrs
    dta
}

#' @rdname methods.dfidx
#' @method subset dfidx
#' @export
subset.dfidx <- function(x, subset, select, drop = FALSE, drop.unused.levels = TRUE, ...){
    attrs <- attributes(x)
    .idx <- idx(x)
    idx.pos <- unname(idx_name(x))
    .first <- idx.pos == 1L
    idx.nm <- names(idx_name(x))
    x <- unfold_idx(x)
    idx.vctr <- attr(x, "idx_vector")
    .idx.series <- c(names(idx.vctr), unname(idx.vctr))
    .idx.series <- setdiff(.idx.series, "")
    class(x) <- setdiff(class(x), c("dfidx", "tbl_dfidx"))
    chkDots(...)
    r <- if (missing(subset)){
             rep_len(TRUE, nrow(x))
         } else {
             e <- substitute(subset)
             r <- eval(e, x, parent.frame())
             if (is.logical(r)){
                 r & !is.na(r)
             } else {
                 if (! is.numeric(r)){
                     stop("subset should be logical or numeric")
                 } else {
                     r <- as.integer(r)
                     v <- rep_len(FALSE, nrow(x))
                     v[r] <- TRUE
                     v
                 }
             }
         }
    vars <- if (missing(select)){
                names(x)
            }  else {
                nl <- as.list(seq_along(x))
                names(nl) <- names(x)
                eval(substitute(select), nl, parent.frame())
            }
    vars <- setdiff(vars, idx.nm)
    vars <- union(vars, .idx.series)
    x <- x[r, vars, drop = drop]
    .pos <- ifelse(idx.pos == 1L, 1L, length(x) - length(.idx.series)+ 1L)
    x <- dfidx(x, idx.vctr, position = .pos, name = idx.nm, drop.unused.levels = drop.unused.levels)
    attr(x, "clseries") <- attrs$clseries
    class(x) <- attrs$class
    x
}

#' @rdname methods.dfidx
#' @export
organize <- function(x, ...){
    .clseries <- attr(x, "clseries")
    .class <- attr(x, "class")
    x <- unfold_idx(x)
    rows <- eval.parent(substitute(with(x, order(...))))
    x <- `[`(x, rows, , drop = FALSE)
    x <- fold_idx(x)
    attr(x, "clseries") <- .clseries
    class(x) <- .class
    x
}

