#' Nodes relationship
#'
#' For any two nodes in rooted bifurcating phylogeny, get ancestral nodes and
#' return an output based on this: vector of nodes forming path through tree
#' between taxa, node representing their most recent common ancestor, or the
#' vector of nodes representing their shared evolutionary history.
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a data frame is generated using ggtree::fortify
#'
#' @param tree_dat Data frame describing tree structure
#' @param taxa The taxa from which to explore relationship
#' @param nodes The nodes from which to explore relationship (if use_nodes != TRUE)
#' @param use_nodes Select TRUE to use node numbers rather than taxa labels
#' @param relationship The type of relationship to return ('path', 'mrca' or 'shared' )
#'
#' @return Nodes reflecting an aspect of evolutionary relationship
#' @export
#'
#' @examples
nodes_relationship <- function(tree_dat = NA, taxa = NA, nodes = NA, use_nodes = F,
                               relationship = 'path') {

  if (relationship %in% c('path', 'mrca', 'shared') == FALSE) {
    stop('method must be one of "path", "mrca" or "shared"')
  }

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


  ancestors_A <- node_ancestors(tree_dat, node_id = node_A,
                                keep_target_node = TRUE)
  ancestors_B <- node_ancestors(tree_dat, node_id = node_B,
                                keep_target_node = TRUE)

  if (relationship == 'path') {
    result <- setdiff(union(ancestors_A, ancestors_B),
                      intersect(ancestors_A, ancestors_B))
  }

  if (relationship == 'mrca') {
    result <- intersect(ancestors_A, ancestors_B)[1]
  }

  if (relationship == 'shared') {
    result <- intersect(ancestors_A, ancestors_B)
  }

  result
}
