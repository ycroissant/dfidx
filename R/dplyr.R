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
#' mn <- dfidx(munnell)
#' select(mn, - gsp, - water)
#' mutate(mn, lgsp = log(gsp), lgsp2 = lgsp ^ 2)
#' transmute(mn, lgsp = log(gsp), lgsp2 = lgsp ^ 2)
#' arrange(mn, desc(unemp), labor)
#' filter(mn, unemp > 10)
#' pull(mn, gsp)
#' slice(mn, c(1:2, 5:7))
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
#' @method arrange dfidx
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
#' @method filter dfidx
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
#' @method slice dfidx
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
#' @method mutate dfidx
#' @export
mutate.dfidx <- function(.data, ...){
    attrs <- attributes(.data)
#    .data <- as.data.frame(.data)
    .data <- unfold_idx(.data)
    .data <- mutate(.data, ...)
    .data <- fold_idx(.data)
#    attrs$names <- names(.data)
#    attributes(.data) <- attrs
    .data
}

#' @rdname dplyr
#' @importFrom dplyr transmute
#' @method transmute dfidx
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
#' @method select dfidx
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


