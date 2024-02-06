#' Tree slice diversity
#'
#' Work with a tree and a dataframe describing its structure to calculate the
#' diversity of tips sampled within a window of time. Diversity is calculated
#' as the mean of the patristic or phylogenetic distances between tips sampled
#' during a window in time.
#'
#' @param tree_dat Data frame describing tree structure
#' @param tree Phylo class object for tree associated with tree_dat
#' @param date_start Lower date limit for temporal slice
#' @param date_end Upper date limit for temporal slice
#' @param cond_var Column in tree_dat to use for selection
#' @param cond_val Value in cond_var column to use for selection
#'
#' @return mean pairwise patristic distance between tips in time slice
#' @export
tree_slice_diversity <- function(tree_dat = NA, tree = NA, date_start = NA,
                                 date_end = NA, cond_var = NA, cond_val = NA) {

  ### input check
  required <- c('node', 'parent', 'isTip','date_frac')
  tree_dat_names <- names(tree_dat)
  if (length(intersect(required, tree_dat_names)) != length(required)) {
    stop('Error: tree_dat does not have full set of required column names:\n',
         paste(required, collapse = ', '))
  }

  if (date_start > date_end) {
    stop('Error: date_start cannot be greater than date_end')
  }
  ### input check

  ## identify relevant tips from which to generate subtree
  tips_in_slice <- tree_dat$node[tree_dat$isTip == T &
                                   tree_dat$date_frac >= date_start &
                                   tree_dat$date_frac < date_end]

  #### option to define tip set with some criteria in addition to time
  if (is.na(cond_var) == FALSE) {
    tips_in_slice <- tree_dat$node[tree_dat$isTip == T &
                                     tree_dat$date_frac >= date_start &
                                     tree_dat$date_frac < date_end &
                                     tree_dat[[cond_var]] == cond_val]
  }
  #### option to define tip set with some criteria in addition to time

  tip_labs_in_slice <- tree_dat$label[tree_dat$node %in% tips_in_slice]


  if (length(tip_labs_in_slice) > 1) {
    # patristic distances for tips in slice
    pd <- ape::cophenetic.phylo(tree@phylo)
    pd <- pd[tip_labs_in_slice, tip_labs_in_slice]
    # calculate mean patristic distance excluding the diagonal
    pd_mean <- mean(pd[row(pd)!=col(pd)])
  } else {
    pd_mean <- NA
  }

  pd_mean
}
