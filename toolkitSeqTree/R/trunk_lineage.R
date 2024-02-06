#' Trunk lineage
#'
#' Identify the branches/nodes that form the trunk lineage where the trunk is
#' defined as the set of branches/nodes that are ancestral to all viruses
#' sampled within a period (defaults to 1) of the most recent sample.
#'
#' @param tree_dat Data frame describing tree structure
#' @param period Period of recent samples whose shared ancestry forms trunk
#'
#' @return set of branches/nodes in trunk lineage
#' @export
trunk_lineage <- function(tree_dat = NA, period = 1) {

  # identify tips within recent time period
  date_cutoff <- max(tree_dat$date_frac) - period
  tips_relevant <- tree_dat$node[tree_dat$isTip == TRUE &
                                   tree_dat$date_frac > date_cutoff]

  # get all ancestral nodes for first tip in recent set
  ancestors_shared <- node_ancestors(tree_dat, node_id = tips_relevant[1])

  # now loop through remaining recent tips retaining the intersect of ancestors
  for (i in 2:length(tips_relevant)) {
    ancestors_new <- node_ancestors(tree_dat, node_id = tips_relevant[i])
    ancestors_shared <- intersect(ancestors_shared, ancestors_new)
  }

  ancestors_shared
}
