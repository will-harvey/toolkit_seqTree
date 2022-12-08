#' Merge alignments
#' With a list of fasta files, function reads in each one, runs the function
#' process_alignment(). process_alignment() will trim sequences and remove
#' insertions as well as tidying names/sequences. If the length of tidied
#' sequences match, function will then rbind() alignments together.
#'
#' @param alignment_files list of fasta files
#' @param rm.duplicates passed to process_alignment()
#' @param isolate.id.column passed to process_alignment()
#' @param suffix passed to process_alignment()
#'
#' @return dataframe with tidied genetic data
#' @export
merge_alignments <- function(alignment_files = files_pb2,
                             rm.duplicates = TRUE,
                             isolate.id.column = TRUE,
                             suffix = NA) {

  # loop through files in 'alignment_files'
  for (i in 1:length(alignment_files)) {
    seq <- phylotools::read.fasta(alignment_files[i])
    # process alignment sorting name, date, trimming sequences
    seq <- process_alignment(seq,
                             rm.duplicates = rm.duplicates,
                             isolate.id.column = isolate.id.column,
                             suffix = suffix)

    # For first file assign output and record sequence length
    if (i == 1) {
      assign('output', seq)
      sequence_column <- names(seq)[grepl('^seq.text', names(seq))]
      seq_length <- stringr::str_length(seq[[sequence_column]][1])
    }

    # For remaining files, check sequence length and rbind
    if (i != 1) {
      curr_seq_length <- stringr::str_length(seq[[sequence_column]][1])
      if (curr_seq_length == seq_length) {
        output <- rbind(output, seq)
      } else {
        stop('Sequence length mismatch. File:\n', alignment_files[i],
             '\nhas length ', curr_seq_length, ' when ',
             seq_length, ' expected.')
      }
    }
  } # finish looping through files
  output
}
