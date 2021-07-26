#' Amino acid distance
#'
#' Given two aligned amino acid sequences, calculate the Grantham distance.
#'
#' @param a The first sequence
#' @param b The second sequence
#' @param dists
#' @param extra.info Option to return more information as a data frame
#'
#' @return Grantham score summed across aa differences in sequences
#' @export
#'
#' @examples
aa_dist <- function(a = NA, b = NA, dists = "grantham", extra.info = F) {

  # deal with lower case input sequences
  a <- toupper(a)
  b <- toupper(b)

  summary <- data.frame(seq_a = strsplit(c(a, b), "")[[1]],
                        seq_b = strsplit(c(a, b), "")[[2]],
                        aa_pair = paste0(strsplit(c(a, b), "")[[1]],
                                         strsplit(c(a, b), "")[[2]]),
                        stringsAsFactors = F)


  #
  if (dists == "grantham") {
    dist_dat <- grantham_dists()
  }
  OUT <- plyr::join(summary, dist_dat, by = "aa_pair")

  # more info (e.g. grantham score per position etc.) if required
  if (extra.info == T) {
    OUT
  } else {
    # If extra info is not required, return only the sum of distances across the sequences
    sum(OUT[["distance"]], na.rm = T)
  }
}
