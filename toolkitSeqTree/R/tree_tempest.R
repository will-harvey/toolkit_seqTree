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
                    c('label','x',  'date')]
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

  ### Scatter plot showing correlation between date distance and
  correlation <- round(cor(res$date, res$root_dist, method = 'pearson'), 3)
  summary(lm(res$root_dist ~ res$date))
  plot <- ggplot2::ggplot(res, ggplot2::aes(x = date, y = root_dist)) +
    ggplot2::geom_point(pch = 21, colour = 'grey20', fill = 'slategrey') +
    geom_smooth(formula = 'y ~ x', method = 'lm', se = F, col = 'grey20') +
    ggplot2::xlab('Date') +
    ggplot2::ylab('Root-to-tip divergence') +
    ggplot2::labs(title = paste("Pearson's correlation coefficient:", correlation)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = element_text(size = 13))

  print(plot)

  res <- summary(lm('x ~ date', tree_dat))
  res_yintercept <- res$coefficients[1]
  res_slope <- res$coefficients[2]
  res_xintercept <- (-res_yintercept) / res_slope
  date_range <- max(tree_dat$date, na.rm = T) - min(tree_dat$date, na.rm = T)

  ######## Print range of info on
  cat('Date range:', round(date_range, 3), '\n')
  cat('Slope (rate):', round(res_slope, 3), '\n')
  cat('x-intercept (tMRCA):', round(res_xintercept, 2), '\n')
  cat('Correlation coefficient:', round(sqrt(res$r.squared), 3), '\n')
  cat('R squared:', round(res$r.squared, 3), '\n')

}


tree_tempest(tree_dat)
