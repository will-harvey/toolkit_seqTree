#' Does trait change
#'
#' With a discrete trait labelled in internal nodes, defined each internal/external
#' node of the phylogeny as representing a change in this trait with respect to
#' its parental node, or not. Optionally, a second column showing the parental
#' trait state and the current trait state of each node.
#'
#' @param tree_dat Data frame describing tree structure
#' @param trait_var Column in tree_dat to assess.
#' @param logical_column Name of logical column returned
#' @param transition_column Name of transition column to be returned if desired
#' @param separator Charachter(s) to separate traits in transition column
#'
#' @return tree_dat with a logical column and optionally a transition column
#' @export
does_trait_change <- function(tree_dat = NA, trait_var = 'trait',
                              logical_column = 'trait_change',
                              transition_column = NULL, separator = "_") {

  # Create column(s) for output
  tree_dat[, logical_column] <- NA
  if (is.null(transition_column) == FALSE) {
    tree_dat[, transition_column] <- NA
  }

  # Loop through rows comparing trait to parental trait
  for (i in 1:nrow(tree_dat)) {
    trait <- tree_dat[i, trait_var]
    parent <- tree_dat$parent[tree_dat$node[i]]
    trait_parent <- tree_dat[tree_dat$node == parent, trait_var]
    tree_dat[i, logical_column] <- trait != trait_parent

    # NA if root
    if (tree_dat$node[i] == tree_dat$parent[i]) {
      tree_dat[i, logical_column] <- NA
    }

    if (is.null(transition_column) == FALSE) {
      tree_dat[i, transition_column] <- as.character(paste(trait_parent,
                                                          trait,
                                                          sep = separator))

      # NA if root
      if (tree_dat$node[i] == tree_dat$parent[i]) {
        tree_dat[i, transition_column] <- NA
      }
    }
  }

  tree_dat
}
