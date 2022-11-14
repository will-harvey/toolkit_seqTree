#' Tree temporal estimate
#'
#' Investigate the temporal signal in a molecular phylogeny.
#' Requires a data frame describing tree structure with columns 'branch.length'.
#' Date info can be supplied as a column 'date' in tree_dat or in a separate data frame.
#'
#' @param tree_dat Data frame describing tree structure, optionally including a 'date' column
#' @param dates Dates can optionally be supplied in a separate data frame
#'
#' @return
#' @export
tree_tempest <- function(tree_dat = NA, dates = NA) {


  if (is.na(dates)) {
    res <- tree_dat[tree_dat$isTip == T,
                    c('label', 'date')]
    res$root_dist <- NA
  }

  ### Need to get a distance from the root for each tip
  tips <- tree_dat$node[tree_dat$isTip == T]
  n_tips <- length(tips)

  # loop through tips
  for (i in 1:n_tips) {
    curr_node <- tips[i]
    # keep_target_node = TRUE to include tip branch length
    ancestors <- node_ancestors(tree_dat, node_id = curr_node,
                                keep_target_node = T)
    # sum branch_lengths for ancestors
    ancestor_lengths <- tree_dat$branch.length[tree_dat$node %in% ancestors]
    res$root_dist[i] <- sum(ancestor_lengths)
  }

  ###
  correlation <- round(cor(res$date, res$root_dist, method = 'pearson'), 3)
  plot <- ggplot2::ggplot(res, ggplot2::aes(x = date, y = root_dist)) +
    ggplot2::geom_point() +
    ggplot2::xlab('Date') +
    ggplot2::ylab('Distance from root') +
    ggplot2::labs(title = paste("Pearson's correlation coefficient:", correlation)) +
    ggplot2::theme_minimal()

  print(plot)
  cat("Pearson's correlation coefficient:\n", correlation)

}
