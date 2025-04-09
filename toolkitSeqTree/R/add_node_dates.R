#' Add node dates
#'
#' Calculate dates of internal nodes of tree given tree structure and the
#' most recent sampling date (mrsd). Can be done for single tree or for a
#' posterior sample of trees (pst).
#'
#' `tree_dat` should have column `x` and in the case of PST, `.id` too.
#'
#' @param tree_dat Data frame describing tree structure
#' @param mrsd date in format "YYYY-MM-DD"
#' @param pst logical, does dataframe represent posterior sample of trees
#'
#' @returns version of tree_dat with columns `date` and `date_format`
#' @export
#'
add_node_dates <- function(tree_dat = NA, mrsd = NA, pst = FALSE) {

  MRSD_decimal <- ggtree::Date2decimal(MRSD)
  tree_dat$date_frac <- NA

  ## if working with PST, need to account for different max x per tree
  if (pst == TRUE) {
    tree_ids <- unique(tree_dat$.id)
    n_trees <- length(tree_ids)

    # loop through trees in PTS
    for (i in 1:n_trees) {
      # id max x for current tree
      curr_tree_id <- tree_ids[i]
      curr_max_x <- max(tree_dat$x[which(tree_dat$.id == curr_tree_id)])
      # if tree id matches curr_tree_id, calc date
      tree_dat$date_frac <- ifelse(tree_dat$.id == curr_tree_id,
                                   MRSD_decimal - (curr_max_x - tree_dat$x),
                                   tree_dat$date_frac)
    }
  } else if (pst == FALSE) {
    max_x <- max(tree_dat$x)
    tree_dat$date_frac <- MRSD_decimal - (max_x - tree_dat$x)
  }

  # also create column with formatted version of date
  tree_dat$date <- ggtree::decimal2Date(tree_dat$date_frac)

  tree_dat
}
