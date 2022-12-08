#' Assign sequence cluster
#'
#' Assign clusters on the basis of sequence similarity using the seq_cluster() function from
#' the bioseq package. Re-label
#'
#' @param seq_vector Character vector representing aligned nucleotide sequences
#' @param threshold threshold similarity to pass to bioseq::seq_cluster()
#' @param method clustering method to pass to bioseq::seq_cluster()
#'
#' @return
#' @export
#'
asn_seq_cluster <- function(seq_vector = NA, threshold = 0.02, method = 'single') {
  # convert to dna object and run cluster algorithm
  seq_dna <- bioseq::dna(seq_vector)
  cluster <- bioseq::seq_cluster(seq_dna, threshold = threshold,
                                 method = method)

  # reorder cluster labels according to cluster frequency
  cluster <- as.data.frame(cluster)
  tab <- as.data.frame(table(cluster$cluster))
  tab <- tab[order(tab$Freq, decreasing = T),]
  cluster$cluster.relabel <- NA
  for (i in 1:nrow(tab)) {
    curr_level <- tab$Var1[i]
    cluster$cluster.relabel <- ifelse(cluster$cluster == curr_level,
                                      i,
                                      cluster$cluster.relabel)
  }
  cluster$cluster.relabel
}
