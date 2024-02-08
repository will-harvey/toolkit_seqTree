#' Tree slice time of most recent common ancestor
#'
#' Given a dataframe describing a phylogenetic tree, find the time of the most
#' recent common ancestor of a temporal slice of the tree. Function can find
#' the time of the most recent common ancestor (tMRCA) of all extant lineages
#' in the temporal slice (method = "complete") or the tMRCA of tips sampled
#' during a temporal slice only (method = "window").
#'
#' @param tree_dat Data frame describing tree structure
#' @param date_start Lower date limit for temporal slice
#' @param date_end Upper date limit for temporal slice
#' @param method Whether to use the "complete" (default) or "window" method
#'
#' @return fractional date for tMRCA
#' @export
tree_slice_tmrca <- function(tree_dat = NA, date_start = NA, date_end = NA,
                             method = 'complete', cond_var = NA, cond_val = NA) {

  ### input check
  required <- c('node', 'parent', 'isTip','date_frac')
  tree_dat_names <- names(tree_dat)
  if (length(intersect(required, tree_dat_names)) != length(required)) {
    stop('Error: tree_dat does not have full set of required column names:\n',
         paste(required, collapse = ', '))
  }

  if (!method %in% c('complete', 'window')) {
    stop('Error: Unrecognised method. Method must be either "complete" or "window"')
  }

  if (date_start > date_end) {
    stop('Error: date_start cannot be greater than date_end')
  }
  ### input check

  # Before working with time slice, calculate fractional date of every parent node
  tree_dat$date_frac_parent <- NA
  for (i in 1:nrow(tree_dat)) {
    node_parent <- tree_dat$parent[i]
    tree_dat$date_frac_parent[i] <- tree_dat$date_frac[tree_dat$node == node_parent]
  }

  #### identify relevant tips from which to generate subtree
  tips_relevant <- c()
  # 1. tip in time slice (only requirement when method is 'window')
  tips_in_slice <- tree_dat$node[tree_dat$isTip == T &
                                   tree_dat$date_frac >= date_start &
                                   tree_dat$date_frac < date_end]


  if (method == 'complete') {
    # 2. tips descended from internal node in time slice
    internal_in_slice <- tree_dat$node[tree_dat$isTip == F &
                                         tree_dat$date_frac >= date_start &
                                         tree_dat$date_frac < date_end]
    # loop through these internals recording descendant tips
    descendent_relevant <- c()
    for (i in 1:length(internal_in_slice)) {
      new <- node_descendants(tree_dat, node_id = internal_in_slice[i], tips_only = T)
      descendent_relevant <- union(descendent_relevant, new)
    }


    # 3. tips descended from branches that traverse time slice
    traverse_dat <- tree_dat[tree_dat$date_frac >= date_end &
                               tree_dat$date_frac_parent < date_start,]
    # get nodes descended from these branches that traverse time slice
    traverse_child <- traverse_dat$node
    # get any tips in this set
    traverse_tips <- traverse_dat$node[traverse_dat$isTip == T]
    # loop through all traverse_child recording descendant tips
    traverse_relevant <- c()
    for (i in 1:length(traverse_child)) {
      new <- node_descendants(tree_dat, node_id = traverse_child[i], tips_only = T)
      traverse_relevant <- union(traverse_relevant, new)
    }
    traverse_relevant <- c(traverse_relevant, traverse_tips)

  }

  ## Tips to use depend on method
  if (method == 'complete') {
    tips_relevant <- union(tips_in_slice, descendent_relevant)
    tips_relevant <- union(tips_relevant, traverse_relevant)
  } else if (method == 'window') {
    tips_relevant <- tips_in_slice
  }

  #### option to define tip set with some criteria in addition to time
  if (is.na(cond_var) == FALSE) {
    tips_relevant <- tree_dat$node[tree_dat$node %in% tips_relevant &
                                     tree_dat[[cond_var]] == cond_val]
  }
  #### option to define tip set with some criteria in addition to time


  if (length(tips_relevant) > 1) {
    ## Determine the MRCA of this set of relevant tips
    mrca_node <- set_mrca(tree_dat, tips_relevant)

    tmrca <- tree_dat$date_frac[tree_dat$node %in% mrca_node]
  } else {
    tmrca <- NA
  }

  tmrca
}
