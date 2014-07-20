#' Convert a vector of FPKM to TPM
#'
#' Given a vector with Fragments Per Kilobase per exon model Million, convert
#' the units to Transcripts Per Million (TPM)
#'
#' @param x a vector of all observed FPKM values
#' @return a vector of all TPM values
fpkmToTpm <- function(x)
{
    lDenom <- log(sum(x))
    exp(log(x) - lDenom + log(1e6))
}
