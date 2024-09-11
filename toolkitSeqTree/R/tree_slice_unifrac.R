#' Tree slice UniFrac calculation
#'
#' Given a dataframe describing a phylogenetic tree, calculate the UniFrac
#' metric considering tips within a temporal slice of the tree. UniFrac is a
#' measure of the phylogenetic signal in a discrete trait.
#'
#' Nodes are inferred to as being unambiguously associated with
#' a single trait level or as being ambiguous. The higher the fraction of
#' unambiguous branches as a proportion of the total branch length in ancestry
#' of tips within a temporal slice, the greater the phylogenetic signal.
#'
#' @param tree_dat Data frame describing tree structure
#' @param date_start Lower date limit for temporal slice
#' @param date_end Upper date limit for temporal slice
#' @param trait_var Name of column with discrete trait
#' @param per_level Logical on whether to calculate a level-specific metric rather than the overall UniFrac metric for the trait
#' @param return_df Logical - should dataframe with inferred internal nodes be returned
#'
#' @return UniFrac metric
#' @export
tree_slice_unifrac <- function(tree_dat = NA, date_start = NA, date_end = NA,
                               trait_var = 'trait', per_level = FALSE,
                               return_df = FALSE, cond_var = NA, cond_val = NA) {

  ### input check
  required <- c('node', 'parent', 'isTip','date_frac')
  tree_dat_names <- names(tree_dat)
  if (length(intersect(required, tree_dat_names)) != length(required)) {
    stop('Error: tree_dat does not have full set of required column names:\n',
         paste(required, collapse = ', '))
  }

  if (date_start > date_end) {
    stop('Error: date_start cannot be greater than date_end')
  }
  ### input check

  #### Identify all branches in ancestry of slice tips

  # identify relevant tips from time slice
  tips_relevant <- tree_dat$node[tree_dat$isTip == T &
                                   tree_dat$date_frac >= date_start &
                                   tree_dat$date_frac < date_end]

  #### option to define tip set with some criteria in addition to time
  if (is.na(cond_var) == FALSE) {
    tips_relevant <- tree_dat$node[tree_dat$node %in% tips_relevant &
                                     tree_dat[[cond_var]] == cond_val]
  }
  #### option to define tip set with some criteria in addition to time

  ancestors_slice <- node_ancestors(tree_dat, tips_relevant[1])
  # Loop through remaining tips in slice, merging their ancestors
  if (length(tips_relevant) > 1) {
    for (i in 2:length(tips_relevant)) {
      ancestors_slice <- union(ancestors_slice,
                               node_ancestors(tree_dat, tips_relevant[i]))
    }
  }

  # for the complete ancestry, bind ancestors and tips in slice
  ancestors_slice <- union(tips_relevant, ancestors_slice)

  # Now, remove any nodes from earlier than the tMRCA of all the tips from the
  # temporal window
  # ID single node that is MRCA of slice
  mrca_slice <- set_mrca(tree_dat, tip_set = tips_relevant)
  # ID ancestors of MRCA
  ancestors_mrca <- node_ancestors(tree_dat, node_id = mrca_slice)
  # use setdiff to remove MRCA and its ancestors from slice ancestors
  ancestors_slice <- setdiff(ancestors_slice, c(mrca_slice, ancestors_mrca))

  # create a variable that indicates if node is in ancestry of tips in slice
  tree_dat$ancestry <- tree_dat$node %in% ancestors_slice



  #### Calculate ambiguous/unambiguous branches
  # can be done for whole tree
  tree_dat <- tips_2_root(tree_dat, trait_var = trait_var,
                          prefer_intersect = FALSE,
                          resolve_unambiguous = TRUE,
                          inferred_column = trait_var)

  # calculate, accounting for ancestry of tip slice
  branches_unambig <- sum(tree_dat$branch.length[is.na(tree_dat[[trait_var]]) == FALSE &
                                                   tree_dat$ancestry == TRUE])

  # all branches should be subtree of ancestry of tips excluding anything
  # pre- the MRCA of all tips in slice
  branches_all <- sum(tree_dat$branch.length[tree_dat$ancestry == TRUE])

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
      trait_levels$N_tips[i] <- sum(tree_dat$node %in% tips_relevant &
                                      tree_dat[[trait_var]] == curr_level)

      # 1. for current level, sum unambiguous branch lengths for trait
      # ... that are also in ancestry of tips from slice
      branches_level <- sum(tree_dat$branch.length[which(tree_dat[[trait_var]] == curr_level &
                                                           tree_dat$ancestry == TRUE)])
      trait_levels$branches_unambig[i] <- branches_level

      # 2. Sum all branch lengths in evolutionary history of tips of current level
      # ... that are also in ancestry of tips from slice

      # need relevant tips for level first
      level_tips <- c(tree_dat$node[tree_dat$node %in% tips_relevant &
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
