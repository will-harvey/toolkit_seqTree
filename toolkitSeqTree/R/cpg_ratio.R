#' CpG ratio calculator
#'
#' Calculate the ratio of observed CpG to expected CpG in a nucleotide sequence.
#'
#' @param sequence String representation of nucleotide sequence
#' @param method 1 or 2 representing 1) Saxonov, Berg & Brutlag or 2) Gardiner-Garden & Frommers
#' @param res_expand Expanded results includes observed and expected CpG
#'
#' @return CpG ratio or alternatively data frame with ratio, observed and expected
#' @export
#'
#' @examples
#' sequence <- 'CATGCTGTGGTCGCGTAGGGTTATTCGTCGTAATTGCAGCT'
#' cpg_ratio(sequence, method = 2, res_expand = T)
cpg_ratio <- function(sequence = NA, method = 1, res_expand = F) {
  sequence <- toupper(sequence)
  n_CpG <- stringr::str_count(sequence, pattern = 'CG')
  n_C <- stringr::str_count(sequence, pattern = 'C')
  n_G <- stringr::str_count(sequence, pattern = 'G')
  seq_length <- stringr::str_length(sequence)

  # Two options for calculating expected
  if (method == 1) {
    exp_CpG <- (((n_C + n_G)/2)^2) / seq_length
  } else if (method == 2) {
    exp_CpG <- (n_C * n_G) / seq_length
  } else {
    stop('Unrecognised method - should be 1 or 2')
  }

  ratio_CpG <- n_CpG / exp_CpG

  if (res_expand == T) {
    return(data.frame('X'= c('Observed CpGs', 'Expected CpGs', 'CpG ratio'),
                      'Val'= c(n_CpG, exp_CpG, ratio_CpG)))
  } else {
    return(ratio_CpG)
  }
}

