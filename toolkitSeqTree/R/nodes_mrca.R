#' Nodes MRCA
#'
#' For any two nodes in rooted birfurcating phylogeny, ID the most recent common ancestor
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a data frame is generated using ggtree::fortify
#'
#' @param tree_dat Data frame describing tree structure
#' @param taxa The nodes from which to ID MRCA
#' @param nodes The nodes from which to ID MRCA (if use_nodes != TRUE)
#' @param use_nodes Whether
#'
#' @return Vector of descendant nodes
#' @export
#'
#' @examples
nodes_mrca <- function(tree_dat = NA, taxa = NA, nodes = NA, use_nodes = F) {

  if (use_nodes == F) {

    if (length(taxa) != 2) {stop('Error: length of "taxa" is not equal to 2')}
    if (taxa[1] %in% tree_dat$label == F) {
      stop('Error: taxa[1] is not present among taxa in tree_dat')
    }
    if (taxa[2] %in% tree_dat$label == F) {
      stop('Error: taxa[2] is not present among taxa in tree_dat')
    }

    node_A <- tree_dat$node[which(tree_dat$label == taxa[1])]
    node_B <- tree_dat$node[which(tree_dat$label == taxa[2])]

  } else if (use_nodes == T) {
    if (length(nodes) != 2) {stop('Error: length of "nodes" is not equal to 2')}
    node_A <- nodes[1]
    node_B <- nodes[2]
  }


  ancestors_A <- node_ancestors(tree_dat, node_id = node_A)
  ancestors_B <- node_ancestors(tree_dat, node_id = node_B)

  mrca <- intersect(ancestors_A, ancestors_B)[1]
  mrca
}
