#' Summarise BEAST BSSVS rates
#'
#' The function summarises information in BEAST generated log file from a run
#' with discrete traits and a BSSVS network.
#'
#' @param rates_dat Data frame version of log file
#' @param threshold Value above which to retain conditional means. Defaults to 0.5
#'
#' @return summary dataframe
#' @export
#'
beast_summarise_rates_bssvs <- function(rates_dat = NA,
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
                      cond = colMeans(mat_cond))
  means$name <- gsub('\\.rates', '', means$name)

  # variable that is rate mean * ind mean
  means$rate_ind <- means$rate * means$ind

  # version of cond nulled for rates below an 'ind' threshold
  means$cond.nulled <- ifelse(means$ind >= threshold_cond,
                              means$cond, NA)

  # extract source and sink from names from beast
  means$source <- gsub('^[a-zA-Z0-9]+\\.', '', means$name) # remove 'text.' from name - should correspond to nanme of discrete trait
  means$source <- gsub('\\.[a-zA-Z0-9]+', '', means$source) # remove trait after '.'
  means$sink <- gsub('^[a-zA-Z0-9]+\\.', '', means$name) # remove 'text.' from name
  means$sink <- gsub('[a-zA-Z0-9]+\\.', '', means$sink) # remove trait before '.'

  diag <- data.frame(name = NA,
                     rate = NA,
                     ind = NA,
                     cond = NA,
                     rate_ind = NA,
                     cond.nulled = NA,
                     source = sort(unique(means$source)),
                     sink = sort(unique(means$source)))

  return(list(means = means, diag = diag))

}
