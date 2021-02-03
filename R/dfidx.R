# immer (MASS) Cefamandole (nmle)

#' Data frames with indexes
#'
#' data frames for which observations are defined by two (potentialy
#' nested) indexes and for which series have thefore a natural tabular
#' representation
#'
#' @name dfidx
#' @aliases dfidx
#' @param data a data frame
#' @param idx an index
#' @param drop.index if `TRUE` (the default), remove the index series
#'     from the data.frame as stand alone series
#' @param as.factor should the indexes be coerced to factors ?
#' @param pkg if set, the resulting `dfidx` object is of class
#'     `c("dfidx_pkg", "dfidx")` which enables to write specific
#'     classes
#' @param fancy.row.names if `TRUE`, fancy row names are computed
#' @param subset a logical which defines a subset of rows to return
#' @param idnames the names of the indexes
#' @param shape either `wide` or `long`
#' @param choice the choice
#' @param varying,sep relevant for data sets in wide format, these
#'     arguments are passed to reshape
#' @param opposite return the opposite of the series
#' @param levels the levels for the second index
#' @param ranked a boolean for ranked data
#' @param ... further arguments
#' @details Indexes are stored as a `data.frame` column in the
#'     resulting `dfidx` object
#' @return an object of class `"dfidx"`
#' @export
#' @importFrom stats reshape as.formula formula terms update relevel
#' @author Yves Croissant
#' @examples
#'
#' data("TravelMode", package = "AER")
#'
#' # the first two columns contain the index
#'
#' TM1 <- dfidx(TravelMode)
#'
#' # explicitely indicate the two indexes using either a vector or a
#' # list of two characters
#' 
#' TM2 <- dfidx(TravelMode, idx = c("individual", "mode"))
#'
#' TM3 <- dfidx(TravelMode, idx = list("individual", "mode"))
#'
#' # rename one or both indexes
#'
#' TM3b <- dfidx(TravelMode, idnames = c(NA, "trmode"))
#'
#' # for balanced data (with observations ordered by the first, then
#' # by the second index
#'
#' # use the name of the first index
#'
#' TM4 <- dfidx(TravelMode, idx = "individual", idnames = c("individual", "mode"))
#'
#' # or an integer equal to the cardinal of the first index
#'
#' TM5 <- dfidx(TravelMode, idx = 210, idnames = c("individual", "mode"))
#'
#' # Indicate the values of the second index using the levels argument
#'
#' TM5b <- dfidx(TravelMode, idx = 210, idnames = c("individual", "mode"),
#' levels = c("air", "train", "bus", "car"))
#'
#' # Nesting structure for one of the index
#'
#' data("JapaneseFDI", package = "mlogit")
#' JapaneseFDI <- dplyr::select(JapaneseFDI, 1:8)
#' JP1b <- dfidx(JapaneseFDI, idx = list("firm", c("region", "country")),
#' idnames = c("japf", "iso80"))
#'
#' # Data in wide format
#'
#' data("Fishing", package = "mlogit")
#' Fi <- dfidx(Fishing, shape = "wide", varying = 2:9, idnames = c("chid", "alt"))
dfidx <- function(data, idx = NULL, drop.index = TRUE, as.factor = NULL, pkg = NULL,
                  fancy.row.names = FALSE, subset = NULL,
                  idnames = NULL, shape = c("long", "wide"), choice = NULL,
                  varying = NULL, sep = ".", opposite = NULL, levels = NULL, ranked = FALSE, ...){
                  # the default class of the resulting data.frame is dfidx
                  # if (is.null(clsgdata)) clsgdata <- "dfidx"
                  # if clseries is not NA, it is xseries if clseries is NULL or
                  # c(clseries, "xseries") otherwise ; if clseries is NA, it is set
                  # to NULL
    
    shape <- match.arg(shape)
    if (! is.null(varying)) shape <- "wide"

    cldata <- match.call(expand.dots = TRUE)
    # Idea borrowed from plm: if no index are provided and the data
    # set is in long format, they are the first two columns of the
    # data.frame

    if (is.null(idx) & shape == "long") idx <- names(data)[1:2]

    # dfidx can be called with element-list arguments from mlogit or
    # mlogit.data. In this case arguments are called and need to be
    # evaluated
    eval_arg <- function(x) if (is.call(x)) eval(x, parent.frame()) else x
    idx <- eval_arg(idx)
    idnames <- eval_arg(idnames)
    varying <- eval_arg(varying)
    opposite <- eval_arg(opposite)
    levels <- eval_arg(levels)
    # ------------------------------------
    # 1/ Subset the data.frame if required
    # ------------------------------------
    if (match("subset", names(cldata), 0)){
        m <- match(c("data", "subset"), names(cldata), 0)
        cldata <- cldata[c(1, m)]
        names(cldata)[2] <- "x"
        cldata[[1]] <- as.name("subset")
        # dfidx is now called with all its arguments in mlogit, even
        # those not set by the user ; in this case subset only if the
        # subset arguments is not NULL
        data <- eval(cldata, parent.frame())
    }
    # ------------------------------------
    # 1. Some pathological cases
    # ------------------------------------

    # a/ idx is NULL and the levels argument is set It supposes that
    # we have a balanced data and we fill the idx argument with the
    # cardinal of the first index
    
    if (shape == "long" & is.null(idx) & ! is.null(levels)){
        L <- length(levels)
        N <- nrow(data)
        if (N %% L != 0) stop("unbalanced data set, set the idx argument")
        else idx <- N / L
    }

    # b/ idx is of the form list(c(NA, "id")) a grouping variable is
    # provided for the first index, which may be the only way to
    # proceed for data in wide format

    grpvar <- NULL
    
    if (! is.null(idx) && (length(idx) == 1) && (is.na(idx[[1]][1]))){
        grpvar <- idx[[1]][2]
        idx <- NULL
    }

    # c/ the data is in wide format and the levels are provided (as
    # they should be guessed from the names of the series, just
    # remove them

    if (shape == "wide" & ! is.null(levels)){
        warning("the levels shouldn't be provided with a data set in wide format")
        levels <- NULL
    }

    # d/ the data is in long format and idx is of the form c(NA,
    # "aseries"), which means that there is no variable to identify
    # the first index. Then just create it

    if (shape == "long" && is.list(idx) && is.na(idx[[1]][1])){
        nalts <- length(unique(data[[idx[[2]]]]))
        nchid <- nrow(data) / nalts
        data$id1 <- rep(1:nchid, each = nalts)
        idx[[1]] <- "id1"
    }

    # --------------------------------------
    # 2/ Get/Set the names of the index series
    # --------------------------------------    
    
    # idvars is a character of length two which indicates the index
    # series. If no idx, this is id1/id2, otherwise, it is the series
    # provided in the idx argument, except when it is an integer, in
    # this case it is id1
    idvars <- NULL

    if (! is.null(idx)){
        idx <- eval_arg(idx)
        if (length(idx) == 1){
            if (is.numeric(idx)) idvars <- c("id1", NA) else idvars <- c(idx[[1]][1], NA)
        }
        if (length(idx) == 2) idvars <- c(idx[[1]][1], idx[[2]][1])
        if (is.na(idvars)[1]) idvars[1] <- "id1"
        if (is.na(idvars)[2]) idvars[2] <- "id2"
    }
    else idvars <- c("id1", "id2")
    
    # idnames are the names of the index series in the resulting
    # data.frame; either the initial names or those provided with the
    # idnames argument.
    if (is.null(idnames)) idnames <- idvars
    else{
        if (length(idnames) == 1) idnames <- c(idnames, idvars[2])
        if (length(idnames) == 2){
            if (is.na(idnames[1])) idnames <- c(idvars[1], idnames[2])
        }
    }

    # --------------------------------------
    # 2/ Reshape in long format if necessary
    # --------------------------------------    

    # the dfidx is in a "wide" format, in this case reshape it
    # in a "long" format. A series can't be the second index as each
    # line is a choice situation. index can be either:

    # - NULL, in this case id1 is constructed before reshape, and id2
    # after reshape

    # - a character of length one: this character defines id1, id2 is
    # constructed by reshape

    # - a character of length 2 or a list containing a character of
    # length 2 (id1 and the nesting variable)

    if (shape == "wide"){
        if (! is.null(idx)){
            if (is.list(idx)){
                if (length(idx) != 1)
                    stop("for data in wide format, providing id2 is irrelevant")
                chid.var <- idx[[1]][1]
            }
            else{
                if (is.character(idx)){
                    if (! length(idx) %in% 1:2)
                        stop("irrelevant length of the index")
                    chid.var <- idx[1]
                    idx <- as.list(idx)
                }
                else stop("index should be either a list or a character")
            }

            chid.name <- chid.var
            chid.var <- data[[chid.name]]
            if (any(duplicated(chid.var))) stop("non-unique values of id1")
        }
        else{
            chid.var <- 1:nrow(data)
            chid.name <- idnames[1]
        }
        # caution, ids should be a series, not the name of a series!
        alt.name <- idnames[2]
        if (! is.null(varying)){
            varying <- eval_arg(varying)
            totibble <- FALSE
            if (inherits(data, "tbl")){
                data <- as.data.frame(data)
                totibble <- TRUE
            }
            data <- reshape(data, varying = varying, direction = "long", sep = sep,
                            timevar = alt.name, idvar = chid.name, ids = chid.var, ...)
            if (totibble) data <- as_tibble(data)
        }
        else{
            id.names <- as.numeric(rownames(data))
            nb.id <- length(id.names)
            data[[chid.name]] <- id.names
            if (! is.factor(data[[choice]])) data[[choice]] <- factor(data[[choice]])
            lev.ch <- levels(data[[choice]])
            data <- data.frame(lapply(data, rep, length(lev.ch)))
            data[[alt.name]] <- rep(lev.ch, each = nb.id)
            row.names(data) <- paste(data[[chid.name]], data[[alt.name]], sep = ".")
        }
        if (! is.null(choice) & ! ranked)
            data[[choice]] <- as.character(data[[choice]]) == as.character(data[[alt.name]])
        if (is.null(idx)) idx <- list(chid.name, alt.name)
        else{
            if (is.list(idx)) idx[[2]] <- alt.name
            else idx <- list(idx, alt.name)
        }
    }

    if (! is.null(grpvar)) idx[[1]] <- c(idx[[1]], grpvar)
    
    # ----------------------------------------
    # 3/ Set the class of the extracted series
    # ----------------------------------------

    if (is.null(pkg)) clseries <- "xseries"
    else clseries <- c(paste("xseries", pkg, sep = "_"), "xseries")
    
    if (! is.null(levels)){
        O <- nrow(data)
        if (O %% length(levels))
            stop(paste("the data must be balanced in order to use",
                       "the levels argument"))
        else{
            if (is.null(idx)) idx <- O / length(levels)
        }
    }            

    # --------------------------
    # 4/ Put the indexes in form
    # --------------------------
    # index is NULL, take the first two columns as indexes

    if (is.null(idx)) idx <- idnames <- list(names(data)[1], names(data)[2])
    else{
        # index is of length 1
        if (length(idx) == 1){
            if (is.numeric(idx)){
                # index is a numeric, the number of entities defined by
                # id1 ; the names of the two generated indexes are given
                # by the idnames vector
                O <- nrow(data) 
                if (O %% idx) stop(paste("the data must be balanced in order to use",
                                         "an integer as index"))
                N2 <- O / idx
                if (is.null(levels)) levels <- 1:N2
                data[[idnames[1]]] <- rep(1:idx, each = N2)
                data[[idnames[2]]] <- rep(levels, idx)
                idx <- list(idnames[1], idnames[2])}
            else{
                    if (is.list(idx)) idx <- idx[[1]]               
                    # index is a list => this is id1, id2 is NA
                    idx <- list(idx, NA)
                }
        }
        else{
            # if of length 2, coerce it to a list (id1, id2)
            if (is.character(idx) && length(idx) == 2) idx <- as.list(idx)
        }
    }
    # get the position of the first category indexes
    posid1 <- match(idx[[1]], names(data))
    if (any(is.na(posid1))) stop(paste("variable(s)",
                                       paste(idx[[1]][is.na(posid1)], collapse = "-"),
                                       "do(es)n't exist"))
                                        # same for the second category indexes if any
    if (length(idx[[2]]) == 1 && is.na(idx[[2]])){
        posid2 <- NULL
    }
    else{
        posid2 <- match(idx[[2]], names(data))
        if (any(is.na(posid2))) stop(paste("variable(s)",
                                           paste(idx[[2]][is.na(posid2)], collapse = "-"),
                                           "do(es)n't exist"))
    }

    # -------------------------------
    # 5/ Set the class of the indexes
    # -------------------------------
    # coerce or not index to factors
    if (is.null(as.factor)) as.factor <- c(FALSE, TRUE)
    if (! is.logical(as.factor)) stop("the as.factor argument should be logical")
    else{
        if (! length(as.factor) %in% 1:2) stop("the length of the as.factor argument should be 1 or 2")
        if (length(as.factor) == 1) as.factor <- rep(as.factor, 2)
    }
    # coerce the indexes as factors if necessary
    is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5)  abs(x - round(x)) < tol
    data[posid1] <- lapply(data[posid1],
                           function(z){
                               if (as.factor[1]   & ! is.factor(z)) z <- as.factor(z)
                               if (! as.factor[1] &   is.factor(z)){
                                   z <- as.character(z)
                                   znum <- as.numeric(z)
                                   if (! any(is.na(znum))){
                                       z <- znum
                                       zint <- is.wholenumber(z)
                                       if (all(zint)) z <- as.integer(z)
                                   }
                               }
                               z
                           })
    if (! is.null(posid2)){
        data[posid2] <- lapply(data[posid2],
                               function(z){
                                   if (as.factor[2]   & ! is.factor(z)){
                                       if (is.null(levels)) z <- factor(z)
                                       else z <- factor(z, levels = levels)
                                   }
                                   if (! as.factor[2] & is.factor(z)) z <- as.character(z)
                                   z
                               })
    }

    # 6/ Sort the data.frame
    # -------------------------------
    posids <- c(rev(posid1), rev(posid2))
    posids <- as.list(data[posids])
    names(posids) <- NULL
    theorder <- as.call(c(as.name("order"), posids))
    theorder <- eval(theorder)
    data <- data[theorder, ]

    # ------------------------------------------------
    # 7/ Create the second index if it is not provided
    # ------------------------------------------------
    if (is.null(posid2)){
        uniqueid <- unique(data[[posid1[[1]]]])
        Tis <- table(data[[posid1[1]]])
        Tis <- Tis[as.character(uniqueid)]
        if (length(unique(Tis)) == 1){
            if (is.null(levels)) levels <- 1:Tis[1]
            data[[idnames[[2]]]] <- rep(levels, length(uniqueid))
        }
        else data[[idnames[2]]] <- Reduce("c", sapply(Tis, seq_len))
        if (as.factor[2]){
            if (is.null(levels)) data[[idnames[2]]] <- factor(data[[idnames[2]]])
            else data[[idnames[2]]] <- factor(data[[idnames[2]]], levels = levels)
        }
        posid2 <- match(idnames[2], names(data))
    }
    
    # ------------------------------------------------------------------------------
    # 8/ Check that each combination of the two indexes defines a unique observation
    # ------------------------------------------------------------------------------
    z <- data[, c(posid1[1], posid2[1])]
    if (nrow(z) != nrow(unique(z)))
        stop("the two indexes don't define unique observations")

    # ----------------------------------------------------
    # 9/ Put in form the choice variable if it is provided
    # ----------------------------------------------------
    if (! is.null(choice)){
        # if the choice argument is set, coerce it to a boolean
        if (is.null(data[[choice]]))
            # stop if it not exists
            stop(paste("variable", choice, "doesn't exist"))
            if (! is.logical(data[[choice]])){
            if (! is.factor(data[[choice]])){
                data[[choice]] <- factor(data[[choice]])
            }
            if (length(levels(data[[choice]])) != 2 & ! ranked)
                # the number of levels should be exactly equal to two
                stop("The choice variable must have exactly two modalities")
            else{
                # nchid is the number of choice situations, the number
                # of occurences of one of the levels of choice should
                # equal nchid, and is coerced to TRUE
                if (! ranked){
                    nchid <- length(unique(data[[posid1[1]]]))
                    data[[choice]] <- as.numeric(data[[choice]]) - 1
                    tbs <- as.numeric(table(data[[choice]]))
                    if (tbs[2] == nchid) data[[choice]] <- as.logical(data[[choice]])
                    else{
                        if (tbs[1] == nchid) data[[choice]] <- ! as.logical(data[[choice]])
                        else stop("impossible to coerce the choice variable to a logical")
                    }
                }
            }
        }
    }
    
    # ---------------------------------------------------------------
    # 10/ Construct the data.frame of the indexes with its attributes    
    # ---------------------------------------------------------------
    idx <- data[, c(posid1, posid2), drop = FALSE]
    idsattr <- c(rep(1, length(posid1)), rep(2, length(posid2)))
    names(idx)[! duplicated(idsattr)] <- idnames
    attr(idx, "ids") <- idsattr
    posids <- which(! duplicated(idsattr))
    if (drop.index){
        data <- data[, - c(posid1, posid2), drop = FALSE]
        if (ncol(data) == 0L) warning(paste("after dropping of index variables, ",
                                            "the dfidx contains 0 columns"))
    }

    if (fancy.row.names) rownames(data) <- paste(idx[[posids[1]]], idx[[posids[2]]], sep = "-")

    # --------------------------
    # 10/ Take the opposite of the required series
    # --------------------------
    if (! is.null(opposite)){
        if (anyNA(match(opposite, names(data)))) stop("some series in the opposite argument don't exist")
        for (i in opposite) data[[i]] <- - data[[i]]
    }

    # --------------------------
    # 10/ Return the dfidx
    # --------------------------

    if (! is.null(pkg)) clsgdata <- c(paste("dfidx_", pkg, sep = ""), "dfidx")
    else clsgdata <- "dfidx"
    class(idx) <- c("idx", "data.frame")
    rownames(data) <- rownames(idx) <- NULL
    # drop the unused levels for the second index
    idx[[idx_name(idx, 2)]] <- idx[[idx_name(idx, 2)]][drop = TRUE]
    data$idx <- idx
    data <- structure(data, class = c(clsgdata, class(data)), clseries = clseries, choice = choice)
    if (ranked) data <- mymlogit2rank(data, choicename = choice)
    data
}

mymlogit2rank <- function(x, choicename, ...){
    .idx <- idx(x)
    alt_name <- idx_name(x, 2)
    id <- idx(x, 1)
    id_name <- idx_name(x, 1)
    alt <- idx(x, 2)
    L <- length(unique(alt))
    N <- nrow(x) / L
    x <- as.data.frame(x)[- idx_name(x)]
    x <- cbind(x, .idx)
    achoice <- function(x, l, choicename){
        nx <- x[! x[[choicename]] %in% seq_len(l - 1), ]
        nx[[choicename]] <- ifelse(nx[[choicename]] == l, TRUE, FALSE)
        as.data.frame(nx)
    }
    for1id <- function(x, oneid){
        x <- x[x[[id_name]] == oneid, ]
        Reduce("rbind", lapply(seq_len(L - 1), function(l) achoice(x, l, choicename)))
    }
    result <- Reduce("rbind", lapply(unique(id), function(i) for1id(x, i)))
    result[["idx1"]] <- rep(1:(N * (L - 1)), rep(L:2, N))
    rownames(result) <- NULL
    dfidx(result, idx = list(c("idx1", id_name), alt_name), pkg = "mlogit")
}


#' Index for dfidx
#'
#' The index of a `dfidx` is a dat .frame containing the different
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
#' @examples
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
#' 
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

#' Methods for dplyr verbs
#'
#' methods of `dplyr` verbs for `dfidx` objects.  Default functions
#' don't work because most of these functions returns either a
#' `tibble` or a `data.frame` but not a `dfidx`
#' @name dplyr
#' @param .data a dfidx object,
#' @param ... further arguments
#' @return an object of class `"dfidx"`
#' @author Yves Croissant
#' @details These methods always return the data frame column that
#'     contains the indexes and return a `dfidx` object.
#' @examples
#' data("TravelMode", package = "AER")
#' TM <- dfidx(TravelMode)
#' select(TM, - wait, - vcost)
#' mutate(TM, inc2  = income ^ 2, linc = log(income))
#' transmute(TM, inc2  = income ^ 2, linc = log(income))
#' arrange(TM, desc(size), income)
#' filter(TM, income > 35, size <= 2)
#' pull(TM, income)
#' slice(TM, c(1:2, 5:7))
NULL

#' @importFrom dplyr as_tibble
#' @export
dplyr::as_tibble


#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @importFrom dplyr arrange
#' @export
dplyr::arrange

#' @importFrom dplyr slice
#' @export
dplyr::slice

#' @importFrom dplyr pull
#' @export
dplyr::pull

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom dplyr transmute
#' @export
dplyr::transmute

#' @importFrom dplyr select
#' @export
dplyr::select

#' @rdname dplyr
#' @importFrom dplyr arrange
#' @export
arrange.dfidx <- function(.data, ...){
    attrs <- attributes(.data)
    .data <- as.data.frame(.data)
    .data <- arrange(.data, ...)
    attributes(.data) <- attrs
    .data
}

#' @rdname dplyr
#' @importFrom dplyr filter
#' @export
filter.dfidx <- function(.data, ...){
    attrs <- attributes(.data)
    .data <- as.data.frame(.data)
    .data <- filter(.data, ...)
    attrs$row.names <- 1:nrow(.data)
    attributes(.data) <- attrs
    .data
}

#' @rdname dplyr
#' @importFrom dplyr slice
#' @export
slice.dfidx <- function(.data, ...){
    attrs <- attributes(.data)
    .data <- as.data.frame(.data)
    .data <- slice(.data, ...)
    attrs$row.names <- 1:nrow(.data)
    attributes(.data) <- attrs
    .data
}

#' @rdname dplyr
#' @importFrom dplyr mutate
#' @export
mutate.dfidx <- function(.data, ...){
    attrs <- attributes(.data)
    .data <- as.data.frame(.data)
    .data <- mutate(.data, ...)
    attrs$names <- names(.data)
    attributes(.data) <- attrs
    .data
}

#' @rdname dplyr
#' @importFrom dplyr transmute
#' @export
transmute.dfidx <- function(.data, ...){
    idxpos <- idx_name(.data)
    idx <- .data[[idxpos]]
    attrs <- attributes(.data)
    .data <- as.data.frame(.data)
    .data <- transmute(.data, ...)
    .data[[names(idxpos)]] <- idx
    attrs$names <- names(.data)
    attributes(.data) <- attrs
    .data
}

#' @rdname dplyr
#' @importFrom dplyr select
#' @export
select.dfidx <- function(.data, ...){
    idxpos <- idx_name(.data)
    attrs <- attributes(.data)
    x <- as.data.frame(.data)
    x <- select(x, ...)
    # idx should be a sticky column; if not selected, add it to the
    # resulting data.frame
    if (! names(idxpos) %in% names(x)){
        idx <- .data[[idxpos]]
        x[[names(idxpos)]] <-  idx
    }
    attrs$names <- names(x)
    attributes(x) <- attrs
    x
}

#' model.frame/matrix for dfidx objects
#'
#' Specific model.frame/matrix are provided for dfidx objects. This
#' leads to an unusual order of arguments compared to the
#' usage. Actually, the first two arguments of the model.frame method
#' are a dfidx and a formula and the only main argument of the
#' model.matrix is a dfidx which should be the result of a call to the
#' model.frame method, i.e. it should have a term attribute.
#' @param formula a `dfidx`
#' @param data a `formula`
#' @param ...,lhs,rhs,dot see the `Formula` method
#' @param alt.subset a subset of levels for the second index
#' @param reflevel a user-defined first level for the second index
#' @param balanced a boolean indicating if the resulting data.frame
#'     has to be balanced or not
#' @importFrom Formula as.Formula Formula
#' @importFrom stats model.matrix
#' @importFrom stats model.frame
#' @return a `dfidx` object for the `model.frame` method and a matrix
#'     for the `model.matrix` method.
#' @export
#' @author Yves Croissant
#' @examples
#' data("TravelMode", package = "AER")
#' TM <- dfidx(TravelMode)
#' mf <- model.frame(TM, choice ~ vcost | income - 1 | travel)
#' head(model.matrix(mf, rhs = 1))
#' head(model.matrix(mf, rhs = 2))
#' head(model.matrix(mf, rhs = 1:3))
model.frame.dfidx <- function(formula, data = NULL, ...,
                              lhs = NULL, rhs = NULL, dot = "previous",
                              alt.subset = NULL, reflevel = NULL,
                              balanced = FALSE){
    # get the data and the formula (in this unusual order)
    .data <- formula
    # coerce the formula to a Formula object if necessary
    if(inherits(data, "Formula")) .formula <- data else .formula <- as.Formula(data)
    # update the formula in order to include the indexes in the last part
    .oformula <- .formula
    .formula <- add_idx(.formula, .data)
    # coerce the data to a data.frame (index series become just
    # ordinary series) with an ids attribute (a data.frame with the
    # index series and a digit 1/2 indicating to which index they
    # refer to)
    .choice <- attr(.data, "choice")
    if (class(.data)[1] != "dfidx") .pkg <- strsplit(class(.data)[1], "_")[[1]][2]
    else .pkg <- NULL
    .data <- unfold_idx(.data)
    .ids <- attr(.data, "ids")
    # use the Formula's model.frame method
    .data <- model.frame(.formula, .data, ...,
                         lhs = lhs, rhs = rhs, dot = dot)
    .nterms <- attr(.data, "terms")
    # add the previously saved ids attribute
    attr(.data, "ids") <- .ids
    attr(.data, "choice") <- .choice
    # "fold" the index in a data.frame column to get an dfidx object
    .data <- fold_idx(.data, pkg = .pkg)
    # select a subset of alternatives if requires, which implies
    # removing the choice situations for which the chosen alternative
    # is not in the subset
    if (! is.null(alt.subset) | balanced){
        .idx <- idx(.data)
    }
    if (! is.null(alt.subset)){
        choice <- attr(.data, "choice")
        if (is.null(choice)){
            #stop("the use of alt.subset requires that a choice variable is defined")
            choice <- paste(deparse(.formula[[2]]))
        }
        .idx <- idx(.data)
        name_id1 <- idx_name(.data, 1)
        name_id2 <- idx_name(.data, 2)
        id1 <- .idx[[name_id1]]
        id2 <- .idx[[name_id2]]
        chid_kept <- subset(.idx, .data[[choice]] & id2 %in% alt.subset)[[1]]
        rows_kept <- (id1 %in% chid_kept) & (id2 %in% alt.subset)
        .data <- .data[rows_kept, ]
        .data[[idx_name(.data)]][[idx_name(.data, 2)]] <-
            .data[[idx_name(.data)]][[idx_name(.data, 2)]][drop = TRUE]
    }
    if (balanced){
        .idx <- idx(.data)
        name_id1 <- idx_name(.data, 1)
        name_id2 <- idx_name(.data, 2)
        id1 <- .idx[[name_id1]]
        id2 <- .idx[[name_id2]]        
        un_id1 <- unique(id1)
        un_id2 <- unique(id2)
        complete <- expand.grid(un_id2, un_id1, stringsAsFactors = FALSE)[, 2:1]
        names(complete) <- c(idx_name(.data, 1), idx_name(.data, 2))
        .ids <- attr(.idx, "ids")
        complete <- merge(complete, unique(.idx[.ids == 1]), all.x = TRUE)
        .data <- unfold_idx(.data)
        .ids <- attr(.data, "ids")
        .data <- merge(.data, complete, all.y = TRUE)
#        .data <- .data[order(.data[[name_id1]], .data[[name_id2]]),]
        attr(.data, "ids") <- .ids
        .data <- fold_idx(.data, pkg = .pkg)
    }
    if (! is.null(reflevel)){
        .levels <- levels(idx(.data)[[idx_name(.data, 2)]])
        if (! reflevel %in% .levels) stop("unknown reference level")
        else .data[[idx_name(.data)]][[idx_name(.data, 2)]] <-
                 relevel(.data[[idx_name(.data)]][[idx_name(.data, 2)]], reflevel)
    }
    else .levels <- NULL
    
    attr(.data, "terms") <- .nterms
    attr(.data, "formula") <- .oformula
    attr(.data, "alt.ordering") <- .levels
    .data
}

#' @rdname model.frame.dfidx
#' @param object a dfidx object
#' @export
model.matrix.dfidx <- function(object, ..., lhs = NULL, rhs = 1, dot = "separate"){
    if (is.null(attr(object, "formula")))
        stop("the argument is an ordinary dfidx object")
    .formula <- attr(object, "formula")
    model.matrix(.formula, object, lhs = lhs, rhs = rhs, dot = dot)
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
#' data("TravelMode", package = "AER")
#' TM <- dfidx(TravelMode)
#' TM2 <- unfold_idx(TM)
#' attr(TM2, "ids")
#' TM3 <- fold_idx(TM2)
#' identical(TM, TM3)
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
    
levels.dfidx <- function(x){
    x <- x[[idx_name(x)]][[idx_name(x, 2)]]
    if (is.factor(x)) levels(x) else NULL
}

levels.idx <- function(x){
    x <- x[[idx_name(x, 2)]]
    if (is.factor(x)) levels(x) else NULL
}




