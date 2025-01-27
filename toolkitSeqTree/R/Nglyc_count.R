#' N-linked glycosylation site count
#'
#' Given an amino acid sequence, count the number of potential N-linked glycosylation sites.
#' The N-linked glycosylation motif is N-X-S/T or Asn-X-Ser/T where X is any amino acid
#' except Proline (Pro/P).
#'
#' @param sequence string representing amino acid sequence
#' @param allow_dash logical, should dash be allowed in the middle position of the motif
#' @param allow_X logical, should X be allowed in the middle position of the motif
#'
#' @return the count of N-linked glycosylation sites identified
#' @export
#'
#' @examples
#' sequence <- 'MDTICIGHAYNNPALEKNGSTD'
#' Nglyc_count(sequence)
Nglyc_count <- function(sequence, allow_dash = F, allow_X = F) {

  sequence <- toupper(sequence)
  # aa single letter code does not include BJOUXZ
  # (U is selenocysteine)
  if (allow_dash == T & allow_X == T) {
    pattern <- 'N[-ACDEFGHIKLMNQRSTVWXY][ST]'
  } else if ((allow_dash == T & allow_X == F)) {
    pattern <- 'N[-ACDEFGHIKLMNQRSTVWY][ST]'
  } else if ((allow_dash == F & allow_X == T)) {
    pattern <- 'N[ACDEFGHIKLMNQRSTVWXY][ST]'
  } else if ((allow_dash == F & allow_X == F)) {
    pattern <- 'N[ACDEFGHIKLMNQRSTVWY][ST]'
  }
  # Need lookahead pattern to ID consecutive motifs
  lookahead_pattern <- paste0("(?=(", pattern, "))")

  match_starts <- stringr::str_locate_all(sequence, lookahead_pattern)[[1]]
  match_starts <- match_starts[,"start"]
  x <- length(match_starts)
  return(x)
}

