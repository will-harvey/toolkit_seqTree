#' Tree Balance using Colless index
#'
#' Assess tree balance using the version of the Colless index (Colless, 1982, https://doi.org/10.2307/2413420).
#' which omits normalisation (Shao & Sokal, 1990, https://doi.org/10.2307/2992186).
#' Optionally, a version of the index which considers the imbalence between the
#' size of subtrees descended by each immediate descedant node rather than the
#' imbalence in the number of tips descended can be implemented using
#' `branch_length == T`.
#'
#' @param tree_dat Data frame describing tree structure
#' @param branch_length Logical, implement tree size based version of index
#' @param return_df Logical, return a data frame with balance labelled at each internal node
#'
#' @return Colless index or tree data frame with balance labelled for internal nodes
#' @export
#'
tree_balance_colless <- function(tree_dat = NA, branch_length = F,
                                 return_df = F) {

  ### input check
  required <- c('node', 'parent', 'isTip')
  tree_dat_names <- names(tree_dat)
  if (length(intersect(required, tree_dat_names)) != length(required)) {
    stop('Error: tree_dat does not have full set of required column names:\n',
         paste(required, collapse = ', '))
  }
  ###

  tree_dat$balance <- NA

  ## Loop through tree nodes
  for (i in 1:nrow(tree_dat)) {

    # continue if internal node
    if (tree_dat$isTip[i] == F) {

      # identify child nodes (use setdiff to avoid getting root as parent and child)
      parent <- tree_dat$node[i]
      children <- setdiff(tree_dat$node[tree_dat$parent == parent], parent)
      c1 <- children[1]
      c2 <- children[2]

      # either calculate Colless or branch-length adjusted version
      if (branch_length == FALSE) {
        # id n tips descended from each child
        n_descendants_c1 <- length(node_descendants(tree_dat, c1, tips_only = T))
        n_descendants_c2 <- length(node_descendants(tree_dat, c2, tips_only = T))
        # record absolute difference in number of tips descended
        tree_dat$balance[i] <- abs(n_descendants_c1 - n_descendants_c2)
      } else {
        # id nodes descended from each child
        descendants_c1 <- node_descendants(tree_dat, c1, tips_only = F)
        descendants_c2 <- node_descendants(tree_dat, c2, tips_only = F)
        # calculate subtree size descended from each child
        subtree_size_c1 <- sum(tree_dat$branch.length[tree_dat$node %in% descendants_c1])
        subtree_size_c2 <- sum(tree_dat$branch.length[tree_dat$node %in% descendants_c2])
        # record absolute difference in subtree size
        tree_dat$balance[i] <- abs(subtree_size_c1 - subtree_size_c2)
      }

    }
  }

  if (return_df == TRUE) {
    return(tree_dat)
  } else {
    return(sum(tree_dat$balance, na.rm = T))
  }

}
