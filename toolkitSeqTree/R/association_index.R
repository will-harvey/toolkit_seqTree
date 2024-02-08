#' Association index
#'
#' Calculate the association index (Wang et al. 2001 doi: 10.1128/JVI.75.23.11686-11699.2001)
#'
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a data frame is generated using ggtree::fortify
#' Further requires a column with a discrete trait in which to explore phylogenetic signal
#'
#' @param tree_dat Data frame describing tree structure
#' @param trait_var Name of column with discrete trait
#' @param add_col Logical - whether to return tree_dat with added column showing assocaition metric per node
#'
#' @return Association metric
#' @export
#'
association_index <- function(tree_dat = NA, trait_var = 'trait',
                              add_col = FALSE) {

  tree_dat$association <- NA
  # iterate through internal nodes (i.e. isTip == F)
  internals <- tree_dat$node[tree_dat$isTip == F]
  for (i in internals) {
    curr_node <- tree_dat$node[i]
    # Need N tips descended from current node
    descendants <- node_descendants(tree_dat, node_id = curr_node,
                                    tips_only = TRUE)
    N_descendant_tips <- length(descendants)
    # Need Freq. of most common trait in tips descended from current node
    traits_desc <- tree_dat[tree_dat$node %in% descendants, trait_var]
    freq_common_i <- sum(names(which.max(table(traits_desc))) == traits_desc) / length(descendants)

    AI_i <- (1 - freq_common_i) / (2^(N_descendant_tips-1))
    tree_dat$association[i] <- AI_i
  }

  AI <- sum(tree_dat$association, na.rm = T)
  cat('Association index = ', signif(AI, 3), '\n')

  if (add_col == TRUE) {
    return(list(AI, tree_dat))
  } else {
    return(AI)
  }
}
