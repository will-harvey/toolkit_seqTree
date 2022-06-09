#' Node ancestors
#'
#' For any node in rooted phylogeny, ID all ancestor nodes back to the root.
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a dataframe is generated using ggtree::fortify
#'
#' @param tree_dat Data frame describing tree structure
#' @param node The node from which to ID all ancestors
#'
#' @return Vector of descendant nodes
#' @export
#'
#' @examples
node_ancestors <- function(tree_dat, node_id) {

  ancestors <- tree_dat$parent[tree_dat$node == node_id]

  # while loop - continue adding ancestors until no more exist
  n_ancestors <- length(ancestors)
  stop <- F
  while (stop == F) {
    # identify immediate descendants of all nodes in descendants
    ancestors <- c(ancestors,
                   tree_dat$parent[tree_dat$node %in% ancestors])
    ancestors <- unique(ancestors)

    # has length of offspring grown?
    if (n_ancestors == length(ancestors)) {
      stop <- T
    }
    n_ancestors <- length(ancestors)
  }

  ancestors
}
