#' UniFrac calculation
#'
#' Calculate the UniFrac metric - a measure of the phylogenetic signal in a
#' discrete trait. Nodes are inferred to as being unambiguously associated with
#' a single trait level or as being ambiguous. The higher the fraction of
#' unambiguous branches as a proportion of the total branch length in the tree
#' indicates phylogenetic signal.
#'
#' @param tree_dat Data frame describing tree structure
#' @param trait_var Name of column with discrete trait
#' @param per_level Logical on whether to calculate a level-specific metric rather than the overall UniFrac metric for the trait
#' @param return_df Logical - should dataframe with inferred internal nodes be returned
#'
#' @return UniFrac metric
#' @export
#'
unifrac <- function(tree_dat = NA, trait_var = 'trait', per_level = FALSE,
                    return_df = FALSE) {

  ### input check
  required <- c('node', 'parent', 'branch.length', 'isTip')
  tree_dat_names <- names(tree_dat)
  if (length(intersect(required, tree_dat_names)) != length(required)) {
    stop('Error: tree_dat does not have full set of required column names:\n',
         paste(required, collapse = ', '))
  }

  # unifrac needs solution with umabiguous internal nodes solved by tips_2_root
  # with prefer_intersect = FALSE
  tree_dat <- tips_2_root(tree_dat, trait_var = trait_var,
                          prefer_intersect = FALSE,
                          resolve_unambiguous = TRUE,
                          inferred_column = trait_var)

  branches_unambig <- sum(tree_dat$branch.length[is.na(tree_dat[[trait_var]]) == FALSE])
  branches_all <- sum(tree_dat$branch.length)
  score <- branches_unambig / branches_all

  cat('UNIFRAC score =', signif(score, 3), '\n')

  ### Per-level section
  if (per_level == TRUE) {

    trait_levels <- data.frame(level = sort(unique(tree_dat[[trait_var]])),
                               N_tips = NA,
                               branches_unambig = NA,
                               branches_history = NA,
                               unifrac = NA)

    # loop through trait levels
    for (i in 1:nrow(trait_levels)) {
      curr_level <- trait_levels$level[i]
      trait_levels$N_tips[i] <- sum(tree_dat$isTip & tree_dat[[trait_var]] == curr_level)

      # 1. for current level, sum unambiguous branch lengths for trait
      branches_level <- sum(tree_dat$branch.length[which(tree_dat[[trait_var]] == curr_level)])
      trait_levels$branches_unambig[i] <- branches_level

      # 2. Sum all branch lengths in evolutionary history of tips of current level
      level_tips <- c(tree_dat$node[tree_dat$isTip == TRUE &
                                      tree_dat[[trait_var]] == curr_level])
      level_ancestors <- c()
      # loop through tips for level, adding ancestors
      for (j in 1:length(level_tips)) {
        level_ancestors <- union(level_ancestors,
                                 node_ancestors(tree_dat, node_id = level_tips[j],
                                                keep_target_node = TRUE))
      } # loop through tips for level, adding ancestors

      branches_history <- sum(tree_dat$branch.length[which(tree_dat$node %in% level_ancestors)])
      trait_levels$branches_history[i] <- branches_history

    } # loop through trait levels

    trait_levels$unifrac <- trait_levels$branches_unambig / trait_levels$branches_history

  }
  ### Per-level section

  if (per_level == FALSE & return_df == TRUE) {
    return(list(score, tree_dat))
  } else if (per_level == FALSE & return_df == FALSE) {
    return(score)
  } else if (per_level == TRUE & return_df == TRUE) {
    return(list(trait_levels, tree_dat))
  } else if (per_level == TRUE & return_df == FALSE) {
    return(trait_levels)
  }

}
