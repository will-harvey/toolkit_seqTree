#' Tree persistence
#'
#' Calculate the phylogenetic persistence of categories within a column of tree
#' dat. The column should contain character entries and values should have been
#' generated for internal branches of the phylogeny. For each tip node,
#' persistence associated with the column categorie is the time back up along
#' branches of the tree (function requires time-scaled tree).
#'
#' @param tree_dat Data frame describing structure of a time-resolved phylogeny
#' @param trait Column of tree data on which persistence to be calculated
#' @param cond_var If used, persistence for `trait` is calculated within this variable
#' @param output_colname Name of persistence column in data frame returned
#'
#' @return tree data frame with column of persistence values
#' @export
tree_persistence <- function(tree_dat = NA, trait = NA, cond_var = NA,
                             output_colname = 'persistence') {

  N_tips <- sum(tree_dat$isTip == T)
  tree_dat$persistence <- NA

  # For each tip
  for (i in 1:N_tips) {

    curr_node <- tree_dat$node[i]
    curr_date <- tree_dat$date_frac[i]
    curr_trait <- tree_dat[[trait]][i]

    if (!is.na(cond_var)) {
      curr_cond_var <- tree_dat[[cond_var]][i]
    }

    # identify all ancestor nodes
    ancestors <- node_ancestors(tree_dat, node_id = curr_node)
    # ...restricting to matches of conditional var in tip if requested
    if (!is.na(cond_var)) {
      ancestors_var <- tree_dat[[cond_var]][ancestors]
      ancestors <- ancestors[which(ancestors_var == curr_cond_var)]
    }

    # identify traits at this set of ancestors
    ancestor_traits <- tree_dat[[trait]][ancestors]

    # identify the nodes matching curr_trait
    nodes_interest <- c()
    # if there are some ancestors (possibly matching conditional var)
    if (length(ancestor_traits) > 0) {
      # if the first node in ancestry matches current trait, proceed
      if (ancestor_traits[1] == curr_trait) {
        a <- 1
        # keep adding ancestral nodes to nodes_interest until trait mismatch
        # this avoids retaining deeper matches through multiple switches
        while (a <= length(ancestor_traits) & ancestor_traits[a] == curr_trait) {
          nodes_interest <- c(nodes_interest, ancestors[a])
          a <- a + 1
        }
      }
    }

    # id date of deepest node in matches before transition
    node_interest <- tail(nodes_interest, 1)
    date_transition <- tree_dat$date_frac[tree_dat$node %in% node_interest]

    # if there is a transition in the history of a sample proceed
    # (date_transition will have length > 0)
    if (length(date_transition) == 1) {
      # persistence is difference between sampled date and transition date
      tree_dat$persistence[i] <- curr_date - date_transition
    }

  }


  # rename the new column of persistence values if required
  names(tree_dat)[names(tree_dat) == 'persistence'] <- output_colname

  tree_dat
}
