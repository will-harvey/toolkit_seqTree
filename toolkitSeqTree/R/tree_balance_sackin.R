#' Tree Balance using Sackin index
#'
#' Function assesses tree balance by calculating the Sackin index which sums
#' root-to-tip depth across the tree. By default depth is calculated as the
#' number of edges / branches.
#'
#' @param tree_dat Data frame describing tree structure
#' @param branch_length Logical, calculate tree size based version of index
#' @param return_df Logical, return a data frame with depth labelled at each external node
#'
#' @return sackin index or tree data frame with depth labelled for external nodes
#' @export
#'
tree_balance_sackin <- function(tree_dat = NA, branch_length = F, return_df = F) {

  ### input check
  required <- c('node', 'parent', 'isTip', 'branch.length')
  tree_dat_names <- names(tree_dat)
  if (length(intersect(required, tree_dat_names)) != length(required)) {
    stop('Error: tree_dat does not have full set of required column names:\n',
         paste(required, collapse = ', '))
  }
  ###

  tree_dat$depth <- NA

  ## Loop through tree nodes
  for (i in 1:nrow(tree_dat)) {

    # continue if tip / external node
    if (tree_dat$isTip[i] == T) {

      # identify ancestors of tip
      ancestors <- node_ancestors(tree_dat, tree_dat$node[i], keep_target_node = T)

      if (branch_length == FALSE) {
        tree_dat$depth[i] <- length(ancestors) - 1
      } else {
        # sum branch lengths back to root to get depth
        tree_dat$depth[i] <- sum(tree_dat$branch.length[tree_dat$node %in% ancestors])
      }

    }
  }

  if (return_df == TRUE) {
    return(tree_dat)
  } else {
    return(sum(tree_dat$depth, na.rm = T))
  }

}
