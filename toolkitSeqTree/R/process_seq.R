#' Process sequences
#'
#' Process an vector containing aligned sequences as character strings. Function will remove
#' non-standard bases, optionally trim to protein coding regions, and optionally will remove
#' possible insertions. A threshold can be provided with which to remove insertions. For example
#' if
#'
#' @param seqs vector of aligned sequences in character strings
#' @param trim.start option to trim before first start codon
#' @param trim.end option to trim beyond last stop codon
#' @param insertion.threshold max. proportion of bases with '-' that can survive insertion strip
#'
#' @return
#' @export
#'
#' @examples
process_seq <- function(seqs = NA, trim.start = TRUE, trim.end = TRUE,
                        insertion.threshold = NULL) {
  # convert to lower case and replace non acgt with dash
  seqs <- as.data.frame(seqs)
  names(seqs)[1] <- 'seq.text'
  seqs$seq.text <- tolower(seqs$seq)
  seqs$seq.text <- gsub('[bd-fh-su-z]', '-', seqs$seq.text)

  if (trim.start == T) {
    # trim looks for first possible start codon 'atg'
    start_codon <- as.data.frame(stringr::str_locate(seqs$seq.text, 'atg'))
    # go with consensus location across sequences in alignment
    start_pos <- as.numeric(names(which.max(table(start_codon$start))))
    # use string length to get last position
    stop_pos <- stringr::str_length(seqs$seq.text[1])
    seqs$seq.text <- stringr::str_sub(seqs$seq.text, start_pos, stop_pos)
  } # trim

  if (trim.end == T) {
    # trim looks for first possible stop codon followed by '-' (ignores frame)
    stop_codon <- as.data.frame(stringr::str_locate(seqs$seq.text,
                                                    'tag----------|taa----------|tga----------'))
    # go with consensus location across sequences in alignment
    stop_pos <- as.numeric(names(which.max(table(stop_codon$start)))) + 2
    seqs$seq.text <- stringr::str_sub(seqs$seq.text, 1, stop_pos)
  }

  if (is.na(insertion.threshold) == F) {
    # remove any nucleotide position with - in majority of sequences
    seq_length <- stringr::str_length(seqs$seq.text[1])
    # work backwards from last position in sequence alignment
    for (i in seq_length:1) {
      # id options present in pos i and id most frequenct
      n_gap <- sum(stringr::str_sub(seqs$seq.text, i, i) == '-')
      prop_gap <- n_gap / nrow(seqs)

      # cut position if consensus is '-'
      if (prop_gap > insertion.threshold) {
        seqs$seq.text <- paste0(stringr::str_sub(seqs$seq.text, 1, i-1),
                                stringr::str_sub(seqs$seq.text, i+1,
                                                 stringr::str_length(seqs$seq.text[1])))
      }
    } # loop backwards through positions
  } # remove insertions

  seqs$seq.text
}
