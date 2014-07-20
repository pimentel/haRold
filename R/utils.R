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
