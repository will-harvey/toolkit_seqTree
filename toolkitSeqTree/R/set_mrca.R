#' Set Most Recent Common Ancestor
#'
#' Given a dataframe describing a phylogenetic tree and a set of tips of
#' interest, identify the internal node of the tree which represents the
#' most recent common ancestor (MRCA) of these tips.
#'
#' @param tree_dat Data frame describing tree structure
#' @param tip_set Numeric vector of external/tip nodes
#'
#' @return node number for most recent common ancestor
#' @export
#'
set_mrca <- function(tree_dat = NA, tip_set = NA) {

  ### input check
  required <- c('node', 'parent', 'isTip', 'x')
  tree_dat_names <- names(tree_dat)
  if (length(intersect(required, tree_dat_names)) != length(required)) {
    stop('Error: tree_dat does not have full set of required column names:\n',
         paste(required, collapse = ', '))
  }
  ###

  nodes <- tip_set

  # use one tip in all comparisons
  node_A <- nodes[1]
  ancestors_A <- node_ancestors(tree_dat, node_id = node_A,
                                keep_target_node = TRUE)

  # loop through other tips in set saving
  set_mrca <- c()
  for (i in 2:length(nodes)) {
    node_B <- nodes[i]
    ancestors_B <- node_ancestors(tree_dat, node_id = node_B,
                                  keep_target_node = TRUE)
    # MRCA is the first node that appears in both sets of ancestors
    result <- intersect(ancestors_A, ancestors_B)[1]
    set_mrca <- c(set_mrca, result)
    set_mrca <- unique(set_mrca)
  }

  # identify which mrca in set is earliest
  mrca_dat <- tree_dat[tree_dat$node %in% set_mrca,]
  mrca_dat <- dplyr::arrange(mrca_dat, x)

  mrca_node <- mrca_dat$node[1]
  mrca_node
}
