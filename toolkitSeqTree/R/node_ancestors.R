#' Node ancestors
#'
#' For any node in rooted phylogeny, ID all ancestor nodes back to the root.
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a dataframe is generated using ggtree::fortify
#'
#' @param tree_dat Data frame describing tree structure
#' @param node_id The node from which to ID all ancestors
#'
#' @return Vector of ancestral nodes
#' @export
node_ancestors <- function(tree_dat = NA, node_id = NA,
                           keep_target_node = FALSE) {

  if (length(node_id) > 1) {
    stop('Error: length of node_id cannot be greater than 1')
  }

  # if character is supplied for node_id, ID the node id associated with it
  if (is.character(node_id) == TRUE) {

    if (node_id %in% tree_dat$label == FALSE) {
      stop('Error: taxa supplied is not present in tree_dat$label')
    }

    node_id <- tree_dat$node[which(tree_dat$label == node_id)]
  }

  # start 'ancestors' with the parent of the target node
  ancestors <- tree_dat$parent[tree_dat$node == node_id]

  # if keep_target_node is TRUE, add node_id to ancestors
  if (keep_target_node == TRUE) {
    ancestors <- c(node_id, ancestors)
  }

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
