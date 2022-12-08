#' Process alignment
#'
#' Function performs some standardisation of names, sequences . Function requires as input a
#' data frame with sequence names in column 'seq.name and nucleotides sequences
#'
#' @param seq_dat data frame with columns seq.name and seq.text
#' @param rm.duplicates option to remove duplicates of sequence labels
#' @param isolate.id.column option to return an isolate ID column
#' @param suffix
#' @param seq_trim.start passed to process_seq() - option to trim before first start codon
#' @param seq_trim.end passed to process_seq() - option to trim beyond last stop codon
#' @param seq_insertion.threshold passed to process_seq() - proportion of bases with '-' required to cut position
#'
#' @return
#' @export
#'
#' @examples
process_alignment <- function(seq_dat = NA, rm.duplicates = TRUE,
                              isolate.id.column = FALSE, suffix = NA,
                              seq_trim.start = TRUE, seq_trim.end = TRUE,
                              seq_insertion.threshold = NULL) {

  ### Names
  # Process names removing unusual characters
  seq_dat$seq.name <- toupper(seq_dat$seq.name)
  seq_dat$seq.name <- gsub("'", '', seq_dat$seq.name)
  seq_dat$seq.name <- gsub('"', '', seq_dat$seq.name)
  # Remove 'A_/_' part of subtype notation
  seq_dat$seq.name <- gsub('A_/_', '', seq_dat$seq.name)
  # [^<contents>] matches anything not in the brackets
  seq_dat$seq.name <- gsub('[^-0-9A-Z_/|\\.]', '_', seq_dat$seq.name)

  ### Dates
  # Extract Numbers and dashes from last bit of name
  seq_dat$date <- stringr::str_extract(seq_dat$seq.name, '[0-9-]+$')
  for (i in 1:nrow(seq_dat)) {
    # look for sequences that end in full dates
    date_len <- stringr::str_length(seq_dat$date[i])
    # non-decimal dates should be 10 characters (e.g. 2020-01-01)
    if (date_len == 10) {
      # convert these dates to decimal date
      date_frac <- ggtree::Date2decimal(seq_dat$date[i])
      date_frac <- round(date_frac, 3)
      seq_dat$seq.name[i] <- paste(seq_dat$seq.name[i], date_frac, sep = '|')
    }
  }


  ### Sequences
  # function replaces unusual characters, trims seqs and removes insertions
  seq_dat$seq.text <- process_seq(seq_dat$seq.text,
                                  trim.start = seq_trim.start,
                                  trim.end = seq_trim.end,
                                  insertion.threshold = seq_insertion.threshold)

  # add variable with proportion of bases present
  seq_dat$nuc <- stringr::str_count(seq_dat$seq.text, '[acgt]')
  # convert to proportion by dividing by seq length
  seq_length <- stringr::str_length(seq_dat$seq.text[1])
  seq_dat$nuc <- seq_dat$nuc / seq_length
  # reorder df by proportion of bases present
  seq_dat <- seq_dat[order(seq_dat$nuc, decreasing = T),]

  ### Tidy and return
  ## Remove duplicates if requested
  if (rm.duplicates == T) {
    seq_dat <- seq_dat[!duplicated(seq_dat$seq.name),]
  }

  ## Create column with EPI ID extracted from
  if (isolate.id.column == TRUE) {
    seq_dat$isolate_id <- stringr::str_extract(seq_dat$seq.name, '^EPI_ISL_[0-9]+')

    # if isolate_id column is present, de-duplicate on this too
    if (rm.duplicates == T) {
      seq_dat <- seq_dat[!duplicated(seq_dat$isolate_id),]
    }

  }

  ## Add suffix to sequence and nucleotide columns if requested
  if (is.na(suffix) != T) {
    names(seq_dat) <- gsub('^seq.text$', paste0('seq.text', suffix), names(seq_dat))
    names(seq_dat) <- gsub('^nuc$', paste0('nuc', suffix), names(seq_dat))
  }

  seq_dat
}
