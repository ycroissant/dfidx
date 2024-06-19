#' Index for dfidx
#'
#' The index of a `dfidx` is a data.frame containing the different
#' series which define the two indexes (with possibly a nesting
#' structure). It is stored as a "sticky" data.frame column of the
#' data.frame and is also inherited by series (of class `'xseries'`)
#' which are extracted from a `dfidx`.
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
#' @return a `data.frame` containing the indexes or a series if a
#'     specific index is selected
#' @export
#' @author Yves Croissant
#' @rdname idx
#' @export
#' @examples
#' if (requireNamespace("AER")){
#' data("TravelMode", package = "AER")
#' TM1 <- dfidx(TravelMode)
#' idx(TM1)
#' inc <- TM1$income
#' idx(inc)
#' # get the first index
#' idx(TM1, 1)
#' # get the second index
#' idx(TM1, 2)
#' idx(inc, 2)
#' }
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

#' Get the names of the indexes
#'
#' 
#' This function extract the names of the indexes or the name of a
#' specific index
#'
#' @name idx_name
#' @param x a `dfidx`, a `idx` or a `xseries` object
#' @param n the index to be extracted (1 or 2, ignoring the nesting
#'     variables)
#' @param m if > 1, a nesting variable
#' @return if `n` is `NULL`, a named integer which gives the posititon
#'     of the `idx` column in the `dfidx` object, otherwise, a
#'     character of length 1
#' @export
#' @author Yves Croissant
#' @examples
#' if (requireNamespace("mlogit")){
#' data("JapaneseFDI", package = "mlogit")
#' JapaneseFDI <- dplyr::select(JapaneseFDI, 1:8)
#' JP1b <- dfidx(JapaneseFDI, idx = list("firm", c("region", "country")),
#' idnames = c("japf", "iso80"))
#' # get the position of the idx column
#' idx_name(JP1b)
#' # get the name of the first index
#' idx_name(JP1b, 1)
#' # get the name of the second index
#' idx_name(JP1b, 2)
#' # get the name of the nesting variable for the second index
#' idx_name(JP1b, 2, 2)
#' }
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

#' @rdname idx_name
#' @export
idx_name.xseries <- function(x, n = NULL, m = NULL){
    .idx <- idx(x)
    idx_name(.idx, n = n, m = m)
}


#' Fold and Unfold a dfidx object
#'
#' `fold_idx` takes a dfidx, includes the indexes as stand alone
#' columns, remove the `idx` column and return a data.frame, with an
#' `ids` attribute that contains the informations about the
#' indexes. `fold_idx` performs the opposite operation
#' @param x a `dfidx` object
#' @param pkg if not `NULL`, this argument is passed to `dfidx`
#' @return a `data.frame` for the `unfold_dfidx` function, a `dfidx`
#'     object for the `fold_dfidx` function
#' @export
#' @author Yves Croissant
#' @examples
#' if (requireNamespace("AER")){
#' data("TravelMode", package = "AER")
#' TM <- dfidx(TravelMode)
#' TM2 <- unfold_idx(TM)
#' attr(TM2, "ids")
#' TM3 <- fold_idx(TM2)
#' identical(TM, TM3)
#' }
unfold_idx <- function(x){
    .idx <- idx(x)
#    print(x)
    .terms <- attr(x, "terms")
    # Liming Wang 26 oct 2020, bug for intercept only models
    #    x <- x[, - match("idx", names(x))]
    x <- x[, setdiff(names(x), c('idx')), drop = FALSE]
    K <- length(x)
    x <- cbind(x, .idx)
    structure(x, ids = data.frame(names = names(x)[(K + 1):(K + length(.idx))],
                                  ids = attr(.idx, "ids"),
                                  stringsAsFactors = FALSE),
              terms = .terms)
}

#' @rdname unfold_idx
#' @export
fold_idx <- function(x, pkg = NULL){
    .terms <- attr(x, "terms")
    .choice <- attr(x, "choice")
    .idx <- vector(mode = "list", length = 2)
    for (i in 1:2) .idx[[i]] <- attr(x, "ids")[attr(x, "ids")$ids == i, "names"]
    x <- dfidx(x, .idx, pkg = pkg)
    attributes(x) <- c(attributes(x), terms = .terms, choice = .choice)
    x
}

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
