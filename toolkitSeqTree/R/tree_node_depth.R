#' Node of depth in tree
#'
#' For each node calculate its depth, which is the size of the smaller
#' of the two subtrees descended by either child node, where size is the sum
#' of branch lengths.
#'
#' @param tree_dat Data frame describing tree structure
#'
#' @returns Input data frame with addition of column denoting depth
#' @export
#'
tree_node_depth <- function(tree_dat = NA) {

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

    # continue if internal node
    if (tree_dat$isTip[i] == F) {

      # identify child nodes (use setdiff to avoid getting root as parent and child)
      parent <- tree_dat$node[i]
      children <- setdiff(tree_dat$node[tree_dat$parent == parent], parent)
      c1 <- children[1]
      c2 <- children[2]

      # id nodes descended from each child
      descendants_c1 <- c(c1, node_descendants(tree_dat, c1, tips_only = F))
      descendants_c2 <- c(c2, node_descendants(tree_dat, c2, tips_only = F))
      # calculate subtree size descended from each child
      subtree_size_c1 <- sum(tree_dat$branch.length[tree_dat$node %in% descendants_c1])
      subtree_size_c2 <- sum(tree_dat$branch.length[tree_dat$node %in% descendants_c2])

      # parent-tip distances for all tips descended from each tip
      x_parent <- tree_dat$x[tree_dat$node == parent]
      x_c1 <- tree_dat$x[tree_dat$node %in% descendants_c1 & tree_dat$isTip == TRUE]
      x_c2 <- tree_dat$x[tree_dat$node %in% descendants_c2 & tree_dat$isTip == TRUE]
      delta_x1 <- median(x_c1 - x_parent)
      delta_x2 <- median(x_c2 - x_parent)

      # record depth as the smaller of the two subtrees
      tree_dat$depth[i] <- min(subtree_size_c1*delta_x1,
                               subtree_size_c2*delta_x2)

    } # continue if internal
  } # loop nodes

  tree_dat
}
