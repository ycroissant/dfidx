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
#' @param name name of the `idx` column
#' @param position position of the `idx` column
#' @param ... further arguments
#' @details Indexes are stored as a `data.frame` column in the
#'     resulting `dfidx` object
#' @return an object of class `"dfidx"`
#' @export
#' @importFrom stats reshape as.formula formula terms update relevel
#' @importFrom dplyr relocate
#' @importFrom tidyselect any_of
#' @author Yves Croissant
#' @examples
#' # the first two columns contain the index
#' mn <- dfidx(munnell)
#'
#' # explicitely indicate the two indexes using either a vector or a
#' # list of two characters
#' mn <- dfidx(munnell, idx = c("state", "year"))
#' mn <- dfidx(munnell, idx = list("state", "year"))
#'
#' # rename one or both indexes
#' mn <- dfidx(munnell, idnames = c(NA, "period"))
#'
#' # for balanced data (with observations ordered by the first, then
#' # by the second index
#'
#' # use the name of the first index
#' mn <- dfidx(munnell, idx = "state", idnames = c("state", "year"))
#'
#' # or an integer equal to the cardinal of the first index
#' mn <- dfidx(munnell, idx = 48, idnames = c("state", "year"))
#'
#' # Indicate the values of the second index using the levels argument
#' mn <- dfidx(munnell, idx = 48, idnames = c("state", "year"),
#'             levels = 1970:1986)
#' 
#' # Nesting structure for one of the index
#' mn <- dfidx(munnell, idx = c(region = "state", president = "year"))
#' 
#' # Data in wide format
#' mn <- dfidx(munnell_wide, idx = c(region = "state"),
#'             varying = 3:36, sep = "_", idnames = c(NA, "year"))
#'
#' # Customize the name and the position of the `idx` column
#' #dfidx(munnell, position = 3, name = "index")
dfidx <- function(data, idx = NULL, drop.index = TRUE, as.factor = NULL, pkg = NULL,
                  fancy.row.names = FALSE, subset = NULL,
                  idnames = NULL, shape = c("long", "wide"), choice = NULL,
                  varying = NULL, sep = ".", opposite = NULL, levels = NULL, ranked = FALSE,
                  name, position, ...){
                  # the default class of the resulting data.frame is dfidx
                  # if (is.null(clsgdata)) clsgdata <- "dfidx"
                  # if clseries is not NA, it is xseries if clseries is NULL or
                  # c(clseries, "xseries") otherwise ; if clseries is NA, it is set
                  # to NULL
    if (! is.list(idx) & ! is.null(names(idx))){
        idx <- lapply(1:length(idx), function(i){
            nms_i <- names(idx)[i]
            if (nchar(nms_i) == 0) idx[[i]]
            else c(idx[[i]], names(idx)[i])
        })
    }
    
    .as.factor <- as.factor
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

    # a/ idx is NULL and the levels argument is set, it is assumed
    # that we have a balanced data and we fill the idx argument with
    # the cardinal of the first index
    
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
    if (is.null(.as.factor)) .as.factor <- c(FALSE, TRUE)
    if (! is.logical(.as.factor)) stop("the as.factor argument should be logical")
    else{
        if (! length(.as.factor) %in% 1:2) stop("the length of the as.factor argument should be 1 or 2")
        if (length(.as.factor) == 1) .as.factor <- rep(.as.factor, 2)
    }
    # coerce the indexes as factors if necessary
    is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5)  abs(x - round(x)) < tol
    data[posid1] <- lapply(data[posid1],
                           function(z){
                               if (.as.factor[1] & ! is.factor(z)) z <- as.factor(z)  # as.factor[1]
                               if (! .as.factor[1] &  is.factor(z)){
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
                                   if (.as.factor[2]   & ! is.factor(z)){
                                       if (is.null(levels)) z <- factor(z)
                                       else z <- factor(z, levels = levels)
                                   }
                                   if (! .as.factor[2] & is.factor(z)) z <- as.character(z)
                                   z
                               })
    }

    # -------------------------------
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
        if (.as.factor[2]){
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
    class(idx) <- c("idx", class(idx))
    rownames(data) <- rownames(idx) <- NULL
    # drop the unused levels for the second index
    idx[[idx_name(idx, 2)]] <- idx[[idx_name(idx, 2)]][drop = TRUE]
    # set idx name and position
    if (missing(name)) .name <- "idx" else .name <- name
    if (missing(position)) .position <- ifelse(inherits(data, "tbl_df"), 1, length(data) + 1) else .position <- position
    K <- length(data)
    if (.position > K + 1) stop(cat("position should be <= ", K + 1, "\n"))
    .before <- seq_len(.position - 1)
    .after <- .position:K
#    print(head(data, 3));stop()
    data[[.name]] <- idx
    data <- data %>% relocate(any_of(.name), .before = any_of(.position))
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


