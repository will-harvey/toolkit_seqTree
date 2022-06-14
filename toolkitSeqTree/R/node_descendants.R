#' Node descendants
#'
#' For an internal node in rooted phylogeny, ID all descendant nodes.
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a dataframe is generated using ggtree::fortify
#'
#' @param tree_dat Data frame describing tree structure
#' @param node_id The node from which to ID all descendants
#'
#' @return Vector of descendant nodes
#' @export
#'
#' @examples
node_descendants <- function(tree_dat = NA, node_id = NA) {

  # For non-tip at least two offspring nodes will exist in bifurcating tree
  descendants <- tree_dat$node[tree_dat$parent == node_id]
  descendants <- setdiff(descendants, node_id) # remove root if selected

  # while loop - continue adding descendants until no more exist
  n_descendants <- length(descendants)
  stop <- F
  while (stop == F) {
    # identify immediate descendants of all nodes in descendants
    descendants <- c(descendants,
                   tree_dat$node[tree_dat$parent %in% descendants])
    descendants <- unique(descendants)

    # has length of offspring grown?
    if (n_descendants == length(descendants)) {
      stop <- T
    }
    n_descendants <- length(descendants)
  }

  descendants
}
