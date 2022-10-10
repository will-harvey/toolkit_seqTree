#' Parsimony score
#'
#' Calculate the parsimony score for a discrete trait labelled in tips. This is the
#' minimum number of trait changes required to give the pattern observed in the tips
#' and is a measure of phylogeny and trait correlation. Lower parsimony scores
#' indicate higher phylogeny-trait correlation.
#'
#' @param tree_dat Data frame describing tree structure
#' @param trait_var Name of column with discrete trait
#' @param prefer_intersect Logical to pass to tips_2_root function
#' @param return_df Logical - should dataframe with new columns be returned
#'
#' @return
#' @export
#'
#' @examples
parsimony_score <- function(tree_dat = NA, trait_var = 'trait',
                            prefer_intersect = TRUE, return_df = FALSE) {

  tree_dat <- tips_2_root(tree_dat, trait_var = trait_var,
                          prefer_intersect = prefer_intersect)

  tree_dat <- root_2_tips(tree_dat, trait_var = trait_var)

  tree_dat <- does_trait_change(tree_dat,  trait_var = trait_var)

  score <- sum(tree_dat$trait_change, na.rm = T)
  cat('Parsimony score =', score, '\n')

  if (return_df == TRUE) {
    return(list(score, tree_dat))
  } else {
    return(score)
  }

}
