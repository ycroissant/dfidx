#' Index for dfidx
#'
#' The index of a `dfidx` is a data frame containing the different
#' series which define the two indexes (with possibly a nesting
#' structure). It is stored as a "sticky" data frame column of the
#' `dfidx` object and is also inherited by series (of class
#' `'xseries'`) which are extracted from a `dfidx` object.
#'
#' @param x a `dfidx` or a `xseries`
#' @param n,m `n` is the index to be extracted (1 or 2), `m` equal to
#'     one to get the index, greater than one to get a nesting
#'     variable.
#' @param ... further arguments (for now unused)
#' @param size the number of characters of the indexes for the format
#'     method
#' @details idx is defined as a generic with a `dfidx` and a `xseries`
#'     method.
#' @return a data frame containing the indexes or a series if a
#'     specific index is selected
#' @export
#' @author Yves Croissant
#' @rdname idx
#' @export
#' @examples
#' mn <- dfidx(munnell, idx = c(region = "state", president = "year"))
#' idx(mn)
#' gsp <- mn$gsp
#' idx(gsp)
#' # get the first index
#' idx(mn, 1)
#' # get the nesting variable of the first index
#' idx(mn, 1, 2)
idx <- function(x, n = NULL, m = NULL) UseMethod("idx")

#' @rdname idx
#' @export
idx.dfidx <- function(x, n = NULL, m = NULL){
    idxnames <- idx_name(x)
    x <- as.data.frame(x)
    .idx <- x[[idxnames]]
    idx(.idx, n = n, m = m)
}

#' @rdname idx
#' @export
idx.idx <- function(x, n = NULL, m = NULL){
    if (! is.null(n)){
        if (is.null(m)) m <- 1
        .ids <- attr(x, "ids")
        .idsn <- which(.ids == n)
        if (length(.idsn) < m) x <- NULL
        else x <- x[[.idsn[m]]]
    }
    x
}

#' @rdname idx
#' @export
idx.xseries <- function(x, n = NULL, m = NULL){
    .idx <- attr(x, "idx")
    idx(.idx, n = n, m = m)
}


#' @rdname idx
#' @export
format.idx <- function(x, size = 4, ...){
    ids <- attr(x, "ids")
    x <- x[, ! duplicated(ids)]
    paste(substr(as.character(x[[1]]), 1, size),
          substr(as.character(x[[2]]), nchar(as.character(x[[2]])) - size + 1,
                 nchar(as.character(x[[2]]))), sep = ":")
}

#' Get the name and the position of the index column
#'
#' 
#' This function extract the names of the indexes (along with the
#' position of the `idx` column) or the name of a specific index
#'
#' @name idx_name
#' @param x a `dfidx`, a `idx` or a `xseries` object
#' @param n the index to be extracted (1 or 2, ignoring the nesting
#'     variables)
#' @param m if > 1, a nesting variable
#' @return if `n` is `NULL`, a named integer which gives the position
#'     and the name of the `idx` column in the `dfidx` object,
#'     otherwise, a character of length 1
#' @export
#' @author Yves Croissant
#' @examples
#' mn <- dfidx(munnell, idx = c(region = "state", president = "year"))
#' # get the position of the idx column
#' idx_name(mn)
#' # get the name of the first index
#' idx_name(mn, 1)
#' # get the name of the second index
#' idx_name(mn, 2)
#' # get the name of the nesting variable for the second index
#' idx_name(mn, 2, 2)
idx_name <- function(x, n = 1, m = NULL)
    UseMethod("idx_name")

#' @rdname idx_name
#' @export
idx_name.dfidx <- function(x, n = NULL, m = NULL){
    idxcols <- sapply(x, function(i) "idx" %in% class(i))
    if (sum(idxcols) > 1) stop("More than one idx column")
    if (is.null(n)) which(idxcols)
    else idx_name(idx(x), n = n, m = m)
}

#' @rdname idx_name
#' @export
idx_name.idx <- function(x, n = NULL, m = NULL){
    if (is.null(n)) x <- NULL
    else{
        .ids <- attr(x, "ids")
        if (is.null(m)) m <- 1
        .idsn <- which(.ids == n)
        if (length(.idsn) < m) x <- NULL
        else x <- names(x)[[.idsn[m]]]
    }
    x
}

## idx_name.data.frame <- function(x, n = NULL, m = NULL){
##     idxcols <- sapply(x, function(i) "idx" %in% class(i))
##     if (sum(idxcols) > 1) stop("More than one idx column")
##     if (is.null(n)) which(idxcols)
##     else idx_name(idx(x), n = n, m = m)
## }

#' @rdname idx_name
#' @export
idx_name.xseries <- function(x, n = NULL, m = NULL){
    .idx <- idx(x)
    idx_name(.idx, n = n, m = m)
}


make_idx <- function(x){
    if (inherits(x, "dfidx")) x <- idx(x)
    if (! inherits(x, "idx"))
        stop("the argument should be a dfidx or a idx object")
    .names <- names(x)
    .ids <- attr(x, "ids")
    names(.ids) <- .names
    ones <- sum(.ids == 1)
    twos <- sum(.ids == 2)
    if (ones == 1) .ids <- c(.ids[1], 1, .ids[- 1])
    if (twos == 1) .ids <- c(.ids[1:3], 2)
    .idx <- names(.ids)[c(1, 3)]
    names(.idx) <- names(.ids)[c(2, 4)]
    .idx
}


#' Fold and Unfold a dfidx object
#'
#' `fold_idx` takes a `dfidx` object, includes the indexes as stand
#' alone columns, remove the `idx` column and return a data frame,
#' with an `ids` attribute that contains the informations about the
#' indexes. `fold_idx` performs the opposite operation.
#' @param x a `dfidx` object
#' @param pkg if not `NULL`, this argument is passed to `dfidx`
#' @param sort a boolean, whether the resulting `dfidx` object should
#'     be sorted or not
#' @return a data frame for the `unfold_dfidx` function, a `dfidx`
#'     object for the `fold_dfidx` function
#' @export
#' @author Yves Croissant
#' @examples
#' mn <- dfidx(munnell, idx = c(region = "state", "year"), position = 3, name = "index")
#' mn2 <- unfold_idx(mn)
#' attr(mn, "ids")
#' mn3 <- fold_idx(mn2)
#' identical(mn, mn3)
unfold_idx <- function(x){
    .is_tibble <- inherits(x, "tbl_df")
    .idx_vector <- make_idx(x)
    .idx <- idx(x)
    .idx_name <- idx_name(x)
    class(x) <- setdiff(class(x), "dfidx")
    .terms <- attr(x, "terms")
    # Liming Wang 26 oct 2020, bug for intercept only models
    #    x <- x[, - match("idx", names(x))]
#    x <- x[, setdiff(names(x), names(.idx_name)), drop = FALSE]
    # 2025-05-16 : setdiff extended to the names of the indexes
    x <- x[, setdiff(names(x), c(names(.idx_name), names(.idx))), drop = FALSE]
    K <- length(x)
    ## if (.is_tibble){
    ##     # 2025-05-19 bind_cols doesn't work for tbls with further classes
    ##     class(x) <- class(.idx) <- c("tbl_df", "tbl", "data.frame")
    ##     x <- bind_cols(x, .idx)
    ## } else x <- cbind(x, .idx)
    x <- cbind(x, .idx)
    structure(x,
              idx_vector = .idx_vector,
              idx_name = .idx_name,
              terms = .terms)
}

## unfold_idx <- function(x){
##     .idx <- idx(x)
##     .idx_name <- idx_name(x)
##     class(x) <- setdiff(class(x), "dfidx")
##     .terms <- attr(x, "terms")
##     # Liming Wang 26 oct 2020, bug for intercept only models
##     #    x <- x[, - match("idx", names(x))]
##     x <- x[, setdiff(names(x), names(.idx_name)), drop = FALSE]
##     K <- length(x)
##     x <- bind_cols(x, .idx)
##     structure(x, ids = data.frame(names = names(x)[(K + 1):(K + length(.idx))],
##                                   ids = attr(.idx, "ids"),
##                                   stringsAsFactors = FALSE),
##               terms = .terms)
## }

#' @rdname unfold_idx
#' @export
fold_idx <- function(x, pkg = NULL, sort = FALSE){
    .idx_vector <- attr(x, "idx_vector")
    .idx_name <- attr(x, "idx_name")
    attr(x, "idx_vector") <- attr(x, "idx_name") <- NULL
    x <- dfidx(x, idx = .idx_vector, pkg = pkg,
               position = .idx_name, name = names(.idx_name), sort = sort)
#    .attr <- attributes(x)
#    .attr <- .attr[setdiff(names(.attr), c("idx_vector", "idx_name"))]
#    attributes(x) <- .attr
    x
}

## Ancienne version sans le nom et la position
## fold_idx <- function(x, pkg = NULL){
##     .idx_vector <- attr(x, "idx_vector")
## #    .pos <- attr(x, "name")
## #    .name <- names(attr(x, "name"))
##     x <- dfidx(x, idx = attr(x, "idx_vector"), pkg = pkg)#, position = .pos, name = .name)
##     .attr <- attributes(x)
##     .attr <- .attr[setdiff(names(.attr), c("idx_vector", "name"))]
##     attributes(x) <- .attr
##     x
## }


## fold_idx <- function(x, pkg = NULL){
##     .terms <- attr(x, "terms")
##     .choice <- attr(x, "choice")
##     .idx <- vector(mode = "list", length = 2)
##     for (i in 1:2) .idx[[i]] <- attr(x, "ids")[attr(x, "ids")$ids == i, "names"]
##     x <- dfidx(x, .idx, pkg = pkg)
##     attributes(x) <- c(attributes(x), terms = .terms, choice = .choice)
##     x
## }

# update the formula in order to add the names of the index series
add_idx <- function(formula, data){
    .idx <- paste(". ~ . + ", paste(names(idx(data)), collapse = " + "))
    nparts <- length(formula)[2]
    .idx <- paste(". ~ ", paste(rep(" . | ", nparts), collapse = ""),
                  paste(names(idx(data)), collapse = " + "))
    update(formula, as.formula(.idx))
}

#' @export
levels.dfidx <- function(x){
    x <- x[[idx_name(x)]][[idx_name(x, 2)]]
    if (is.factor(x)) levels(x) else NULL
}

#' @export
levels.idx <- function(x){
    x <- x[[idx_name(x, 2)]]
    if (is.factor(x)) levels(x) else NULL
}


## .onAttach <- function(lib, pkg){
##     packageStartupMessage(
##         paste0("The tidyverse part of the package is now in the tidydfidx package\n",
##                "so that the dfidx package now depends only on a small set of packages\n"),
##         domain = NULL,  appendLF = TRUE)
## }

