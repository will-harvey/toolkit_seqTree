#' Generate branch variables
#'
#' Create columns of logical variables that indicate whether branches
#' of a phylogenetic tree occur in the evolutionary history of a taxon
#' (variable_type = 'identity') or separate two taxa (variable type =
#' 'difference').
#'
#' Function calls functions node_ancestors() and nodes_relationship()
#'
#' @param tree_dat Data frame describing tree structure
#' @param variable_type 'identity' for whether branch occurs in evolutionary history. 'difference' for whether branch separated taxa or not
#' @param prefix prefix to attach to
#'
#' @return Updated data frame with identity or difference phylo variables
#' @export
generate_branch_vars <- function(tree_dat = NA, variable_type = 'identity',
                                 prefix = NA) {

  if (variable_type %in% c('identity', 'difference') == F) {
    stop('Error: variable type must be "identity" or "difference')
  }

  N_taxa <- length(tree_dat$label[tree_dat$isTip == T])
  N_nodes <- length(unique(tree_dat$node))

  ### Identity section
  if (variable_type == 'identity') {
    # Generate output with nrow = n taxa and ncols = n branches
    res <- matrix(NA, nrow = N_taxa, ncol = N_nodes + 1)
    # convert to DF here so that mix of character/logical is possible
    res <- as.data.frame(res)
    res[,1] <- tree_dat$label[tree_dat$isTip == T]

    # Loop through res getting T/F for each branch for each taxa in turn
    for (i in 1:nrow(res)) {
      curr_ancestors <- node_ancestors(tree_dat, node_id = res[i,1],
                                       keep_target_node = T)
      res[i, 2:ncol(res)] <- tree_dat$node %in% curr_ancestors
    } # loop through rows (taxa) complete

    names(res)[1] <- 'label'
    # Name branch cols, with supplied prefix is provided
    if (is.na(prefix)) {
      names(res)[2:ncol(res)] <- paste0('id_', 1:N_nodes)
    } else {
      names(res)[2:ncol(res)] <- paste0(prefix, 1:N_nodes)
    }

  } # end of identity section

  ### Difference section
  if (variable_type == 'difference') {
    res <- matrix(NA, nrow = N_taxa^2, ncol = N_nodes + 2)
    # convert to DF here so that mix of character/logical is possible
    res <- as.data.frame(res)
    combos <- expand.grid(tree_dat$label[tree_dat$isTip == T],
                          tree_dat$label[tree_dat$isTip == T])
    res[,1] <- as.character(combos[,1])
    res[,2] <- as.character(combos[,2])

    # Loop through res getting T/F for each branch in turn
    for (i in 1:nrow(res)) {
      # if the row had non-matching taxa, get the path
      if (res[i,1] != res[i,2]) {
        taxa_interested <- c(res[i, 1], res[i, 2])
        curr_path <- nodes_relationship(tree_dat, taxa = taxa_interested)
      } else {
        curr_path <- c()
      }
      res[i, 3:ncol(res)] <- tree_dat$node %in% curr_path
    } # loop through rows (taxa combos) complete

    names(res)[1] <- 'label_1'
    names(res)[2] <- 'label_2'
    # Name branch cols, with supplied prefix is provided
    if (is.na(prefix)) {
      names(res)[3:ncol(res)] <- paste0('diff_', 1:N_nodes)
    } else {
      names(res)[3:ncol(res)] <- paste0(prefix, 1:N_nodes)
    }

  } # end of difference section

  res
}
