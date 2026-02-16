#' List string differences
#'
#'
#'
#' @param a string a
#' @param b string b
#' @param exclude vector of characters to ignore when identifying differences
#' @param ignore.case logical, whether or not to ignore case in comparison
#'
#' @return list of differences between two strings
#' @export
#'
list_string_diff <- function(a = "ATTCGA-", b = "attTGTT", exclude = c("-","?"),
                             ignore.case = TRUE) {
  if(nchar(a) != nchar(b)) stop("Lengths of input strings differ. Please check your input.")
  if(ignore.case == TRUE) {
    a <- toupper(a)
    b <- toupper(b)
    exclude <- toupper(exclude)
  }

  seq.a <- unlist(strsplit(a, split=""))
  seq.b <- unlist(strsplit(b, split=""))

  diff.d <- rbind(seq.a, seq.b)

  only.diff <- matrix(diff.d[,diff.d[1,] != diff.d[2,]], nrow = 2)

  pos <- which(diff.d[1,] != diff.d[2,])

  only.diff <- as.data.frame(rbind(pos, only.diff))

  if (ncol(only.diff) > 0) {

    only.diff <- as.data.frame(only.diff[, !(only.diff[2,] %in% exclude |
                                               only.diff[3,] %in% exclude)])

    # if columns still exist after exclusions:
    if (ncol(only.diff) > 0) {
      names(only.diff) <- paste0('V.', 1:ncol(only.diff))
    }
  }

  row.names(only.diff) <- c('pos', 'seq.a', 'seq.b')

  return(only.diff)
}
