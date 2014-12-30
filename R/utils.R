#' Find the adjacent pairs in a sequence
#'
#' Given a start and stop, output a paired sequence
#'
#' @param start start position of the sequence
#' @param stop stop position of the sequence
#' @param simplify if true, return a matrix of the list
#' @return a list with the pair sequence
#' @export
#' @examples
#' adjacentPairs(3, 10)
#' adjacentPairs(0, 4)
#' t(adjacentPairs(0, 4, TRUE))
adjacentPairs <- function(start, stop, simplify = FALSE)
{
    if (stop < start)
        stop("'start' must be greater than 'stop'\n")
    first <- start:(stop-1)
    second <- (start+1):stop
    mapply(c, first, second, SIMPLIFY = simplify)
}

#' Perform a full outer join
#'
#' Peform a full outer join using dplyr \code{\link{dplyr::left_join}}. For
#' additional documentation, see \code{\link{dplyr::left_join}}.
#'
#' @param x the first \code{data.frame}
#' @param y the second \code{data.frame}
#' @param by the columns to do the join by.
#' @return a \code{data.frame} with the full outer join
#' @export
outer_join <- function(left, right, by = NULL, ...)
{
    ldf <- dplyr::left_join(df1, right, by = by, copy = FALSE, ...)
    rdf <- dplyr::left_join(df2, left, by = by, copy = FALSE, ...)

    dplyr::union(ldf, rdf)
}
