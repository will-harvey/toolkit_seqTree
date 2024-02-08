#' Local branching index
#'
#' Calculate the local branching index (Neher et al. 2014 eLife doi: 10.7554/eLife.03568).
#' For each node in a phylogeny, the LBI is the exponentially weighted tree length
#' surrounding a node. Higher LBI indicated rapid branching and clade expansion.
#'
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a dataframe is generated using ggtree::fortify
#'
#' @param tree_dat Data frame describing tree structure
#' @param scale Scaling parameter
#'
#' @return tree_dat updated with lbi column
#' @export
local_branching_index <- function(tree_dat = NA, scale = NA) {


  if ('branch.length' %in% names(tree_dat) != 1) {
    stop('Error: "branch.length" is not present as a single column in the tree_dat dataframe')
  }

  tau <- scale

  #### Up messages
  tree_dat$up <- NA
  # Loop through tips
  tips <- tree_dat$node[tree_dat$isTip == T]
  internals <- tree_dat$node[tree_dat$isTip == F]

  for (i in tips) {
    b_i <- tree_dat$branch.length[i]

    # no up messages for tips so unsure if '* 0' at end is required
    up <- tau*(1 - exp(-b_i/tau)) + exp(-b_i/tau) * 0
    # up <- tau*(1 - exp(-b_i/tau)) + exp(-b_i/tau)
    tree_dat$up[i] <- up
  }

  # loop to iterate while NA still exist in tree_dat$up
  while (sum(is.na(tree_dat$up)) > 0) {
    # work through internal nodes, looking for nodes that can be solved
    # nodes above two tips will be solved first
    for (i in internals) {
      # identify unsolved node ...
      if (is.na(tree_dat$up[i]) == T) {
        unsolved <- tree_dat$node[i]
        children <- tree_dat$node[tree_dat$parent == unsolved]
        children <- setdiff(children, unsolved) # special case: removes root from children

        # ... with zero unsolved (non-NA) child nodes
        if (sum(is.na(tree_dat$up[tree_dat$node %in% children])) == 0) {
          up_child1 <- tree_dat$up[tree_dat$node == children[1]]
          up_child2 <- tree_dat$up[tree_dat$node == children[2]]

          b_i <- tree_dat$branch.length[i]
          up <- tau*(1-exp(-b_i/tau)) + exp(-b_i/tau) * sum(up_child1, up_child2)
          tree_dat$up[i] <- up
        } # ... with two solved child nodes
      } # identify unsolved node ...
    } # for loop running through internals
  } # while loop

  #### Down messages
  tree_dat$down <- NA
  tree_dat$down[tree_dat$node == tree_dat$parent] <- 0 # (0 for root)

  # loop to iterate while NA still exist in tree_dat$up
  while (sum(is.na(tree_dat$down)) > 0) {
    # work through nodes, looking for NA nodes that can be solved
    # nodes below root will be solved first
    for (i in c(tips, internals)) {
      # identify unsolved node ...
      if (is.na(tree_dat$down[i]) == T) {
        unsolved <- tree_dat$node[i]
        parent <- tree_dat$parent[i]

        # ... with solved (non-NA) parent node
        if (is.na(tree_dat$down[tree_dat$node == parent]) == F) {
          children <- tree_dat$node[tree_dat$parent == parent]
          children <- setdiff(children, parent) # special case: removes root from children
          sibling <- setdiff(children, unsolved)

          b_ij <- tree_dat$branch.length[i]
          down_parent <- tree_dat$down[tree_dat$node == parent]
          up_sibling <- tree_dat$up[tree_dat$node == sibling]
          down <- tau*(1-exp(-b_ij/tau)) + exp(-b_ij/tau) * (down_parent + up_sibling)
          tree_dat$down[i] <- down
        } # ... with solved (non-NA) parent node
      } # identify unsolved node ...
    } # for loop running through nodes
  } # while loop

  #### LBI per node from up/down messages
  tree_dat$lbi <- NA
  for (i in 1:nrow(tree_dat)) {
    # LBI_i is sum of down message for node i and up messages of child nodes
    if (tree_dat$isTip[i] == F) {
      unsolved <- tree_dat$node[i]
      children <- tree_dat$node[tree_dat$parent == unsolved]
      children <- setdiff(children, unsolved) # special case: removes root from children
      up_child1 <- tree_dat$up[tree_dat$node == children[1]]
      up_child2 <- tree_dat$up[tree_dat$node == children[2]]
      tree_dat$lbi[i] <- tree_dat$down[i] + up_child1 + up_child2
    }

    # if tip, LBI is down message for node i (no up messages from children)
    if (tree_dat$isTip[i] == T) {
      tree_dat$lbi[i] <- tree_dat$down[i]
    }
  }

  tree_dat
}
