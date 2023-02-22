#' Tree temporal estimate
#'
#' Investigate the temporal signal in a molecular phylogeny.
#' Requires a data frame describing tree structure with columns 'branch.length'.
#' Date info can be supplied as a column 'date' in tree_dat or in a separate data frame.
#'
#' @param tree_dat Data frame describing tree structure, optionally including a 'date' column
#' @param dates Dates can optionally be supplied in a separate data frame
#' @param residuals Logical - should residuals for relationship between dates and root-to-tip divergence be added to dataframe and plotted on tree tips
#' @param return_df Logical - should data frame with 'root_dist' and possibly 'temporal_residual' be returned by function
#'
#' @return
#' @export
tree_tempest <- function(tree_dat = NA, dates = NA,
                         residuals = FALSE, return_df = FALSE) {

  # if dates are not supplied separately..
  if (is.na(dates)) {
    # .. check for date column in tree_dat
    if (!'label' %in% names(tree_dat) & 'date' %in% names(tree_dat) ) {
      stop('Function requires columns named "label" and "date"')
    }
  } else {
    # otherwise merge supplied dates df to tree_dat
    tree_dat <- dplyr::left_join(tree_dat, dates, by = 'label')
  }

  # get date into numeric form
  if (is.numeric(tree_dat$date) == FALSE) {
    tree_dat$date <- ggtree::Date2decimal(tree_dat$date)
  }

  ### Get root-to-tip divergence for each tip
  tree_dat$root_dist <- NA
  tips <- tree_dat$node[tree_dat$isTip == T]
  n_tips <- length(tips)

  # loop through tips calculating root-to-tip divergence
  for (i in 1:n_tips) {
    curr_node <- tips[i]
    # keep_target_node = TRUE to include tip branch length
    ancestors <- node_ancestors(tree_dat, node_id = curr_node,
                                keep_target_node = T)
    # sum branch_lengths for ancestors
    ancestor_lengths <- tree_dat$branch.length[tree_dat$node %in% ancestors]
    tree_dat$root_dist[i] <- sum(ancestor_lengths)
  }

  ## Relationship between dates and root-to-tip divergence
  tip_dat <- tree_dat[tree_dat$isTip == T,]
  corr_res <- summary(lm('root_dist ~ date', tip_dat))

  corr_yintercept <- corr_res$coefficients[1]
  corr_slope <- corr_res$coefficients[2]
  corr_xintercept <- (-corr_yintercept) / corr_slope
  corr_coef <- round(sqrt(corr_res$r.squared), 3)
  corr_r2 <- round(corr_res$r.squared, 3)

  date_range <- max(tree_dat$date, na.rm = T) - min(tree_dat$date, na.rm = T)

  ######## Print range of info
  cat('Date range:', round(date_range, 3), '\n')
  cat('Slope (rate):', round(corr_slope, 3), '\n')
  cat('x-intercept (tMRCA):', round(corr_xintercept, 2), '\n')
  cat('Correlation coefficient:', corr_coef, '\n')
  cat('R squared:', corr_r2, '\n')
  ########

  ### Scatter plot showing correlation between date and root-tip divergence
  corr_plot <- ggplot2::ggplot(tip_dat, ggplot2::aes(x = date, y = root_dist)) +
    ggplot2::geom_point(pch = 21, colour = 'grey20', fill = 'slategrey') +
    geom_smooth(formula = 'y ~ x', method = 'lm', se = F, col = 'grey20') +
    ggplot2::xlab('Date') +
    ggplot2::ylab('Root-to-tip divergence') +
    ggplot2::labs(title = paste("Pearson's correlation coefficient:", corr_coef)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = element_text(size = 13))
  print(corr_plot)


  ## Option - residuals for correlation between date and root-tip divergence
  if (residuals == TRUE) {
    tree_dat$temporal_residual <- NA
    tree_dat$temporal_residual[1:n_tips] <- corr_res$residuals

    ### Tree plot with tips coloured by residual
    resid_plot <- ggtree::ggtree(tree_dat) +
      ggtree::geom_tippoint(ggtree::aes(fill = temporal_residual), pch = 21, size = 1.8) +
      ggplot2::scale_fill_gradient2(low = 'dodgerblue', mid = 'white',
                           high = 'firebrick', midpoint = 0,
                           name = 'Residual') +
      ggplot2::theme(text = element_text(size = 13))
    print(resid_plot)
  }

  ## Option - return data frame with columns 'root_dist' and possibly 'temporal_residual'
  if (return_df == TRUE) {
    return(tree_dat)
  }

}
