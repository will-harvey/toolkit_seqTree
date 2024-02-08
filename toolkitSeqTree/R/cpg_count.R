#' CpG count
#'
#' Count CpG in a nucleotide sequence.
#'
#' @param sequence String representation of nucleotide sequence
#'
#' @return CpG count.
#' @export
#'
#' @examples
#' sequence <- 'CATGCTGTGGTCGCGTAGGGTTATTCGTCGTAATTGCAGCT'
#' cpg_count(sequence, method = 2, res_expand = T)
cpg_count <- function(sequence = NA) {
  sequence <- toupper(sequence)
  n_CpG <- stringr::str_count(sequence, pattern = 'CG')
  n_CpG
}
