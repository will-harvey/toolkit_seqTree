#' Summarise BEAST BSSVS rates
#'
#' Summarise info in BEAST generated log file from a run
#' with discrete traits and a BSSVS network.
#'
#' @param rates_dat Data frame version of log file
#' @param mean_rate Mean overall rate for trait transitions across tree
#' @param threshold_cond Value above which to retain conditional means. Defaults to 0.5.
#'
#' @return summary dataframe for transitions and NA matchining dataframe for non-transitions
#' @export
#'
beast_summarise_rates_bssvs <- function(rates_dat = NA,
                                        mean_rate = 1,
                                        threshold_cond = 0.5) {

  # separate into matrices representing rates and ind variables
  names_rates <- names(dat)[grepl('rates', names(dat))]
  names_inds <- names(dat)[grepl('indicators', names(dat))]

  dat_rates <- dat[, names_rates]
  dat_ind <- dat[, names_inds]

  mat_rates <- as.matrix(dat_rates)
  mat_ind <- as.matrix(dat_ind)

  # multiply matrices together to get conditional rates
  mat_cond <- mat_rates * mat_ind

  # extract column means for plot
  means <- data.frame(name = colnames(mat_rates),
                      rate = colMeans(mat_rates),
                      ind = colMeans(mat_ind),
                      rate_ind = NA,
                      cond = colMeans(mat_cond),
                      cond_025 = apply(mat_cond, 2, quantile, probs = 0.025),
                      cond_975 = apply(mat_cond, 2, quantile, probs = 0.975))
  means$name <- gsub('\\.rates', '', means$name)

  # variable that is rate mean * ind mean (for comparison with cond)
  means$rate_ind <- means$rate * means$ind

  # multiply rates by overall mean rate for trait
  means$rate <- means$rate * mean_rate
  means$rate_ind <- means$rate_ind * mean_rate
  means$cond <- means$cond * mean_rate
  means$cond_025 <- means$cond_025 * mean_rate
  means$cond_975 <- means$cond_975 * mean_rate

  # new column = cond NA'd for rates with 'ind' below threshold
  means$cond_nulled <- ifelse(means$ind > threshold_cond,
                              means$cond, NA)

  # extract source and sink from names from beast
  means$source <- gsub('^[a-zA-Z0-9]+\\.', '', means$name) # remove 'text.' from name - should correspond to nanme of discrete trait
  means$source <- gsub('\\.[a-zA-Z0-9]+', '', means$source) # remove trait after '.'
  means$sink <- gsub('^[a-zA-Z0-9]+\\.', '', means$name) # remove 'text.' from name
  means$sink <- gsub('[a-zA-Z0-9]+\\.', '', means$sink) # remove trait before '.'

  diag <- data.frame(name = NA,
                     rate = NA,
                     ind = NA,
                     rate_ind = NA,
                     cond = NA,
                     cond_025 = NA,
                     cond_975 = NA,
                     cond_nulled = NA,
                     source = sort(unique(means$source)),
                     sink = sort(unique(means$source)))

  return(list(means = means, diag = diag))

}
