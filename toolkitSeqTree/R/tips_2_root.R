#' Tips-to-root
#'
#' Perform tips-to-root traversal of phylogenetic tree with labelled discrete
#' trait in tips. Internal nodes will be labelled parsimoniously. At each internal
#' node, a list of trait options will be built. There is an option to return
#' unambiguous node values using this function. An alternative approach is to follow
#' this function with a root-to-tips traversal (see: root_2_tips.R).
#'
#' @param tree_dat Data frame describing tree structure
#' @param trait_var Name of column in trait dat with trait to be reconstructed
#' @param prefer_intersect Favouring intersect generates more unambiguous internal nodes. Defaults to FALSE
#' @param resolve_unambiguous Should a column with inferred trait be returned
#' @param inferred_column If resolve_unambigous is TRUE, the name of the column result is returned in
#'
#' @return
#' @export
#'
#' @examples
tips_2_root <- function(tree_dat = NA, trait_var = 'trait',
                        prefer_intersect = FALSE,
                        resolve_unambiguous = FALSE,
                        inferred_column = 'trait_infer') {

  # create a column in tree_dat that will be filled with options for trait
  tree_dat$trait_opts <- list(NA)

  # loop to keep iterating while nodes remain un-labelled
  while(sum(is.na(tree_dat$trait_opts)) > 0) {
    # work through rows, looking for nodes we can solve options for
    # nodes above two tips will be solved first
    for (i in 1:nrow(tree_dat)) {

      # if trait is known, assign to option
      if (is.na(tree_dat[i, trait_var]) == F) {
        tree_dat$trait_opts[[i]][] <- as.character(tree_dat[i, trait_var])
      }


      # identify unsolved node ...
      if (is.na(tree_dat[i, trait_var]) == T) {
        unsolved <- tree_dat$node[i]
        children <- tree_dat$node[tree_dat$parent == unsolved]
        children <- setdiff(children, unsolved) # special case: removes root from children

        # ... with two solved child nodes
        if (sum(is.na(tree_dat$trait_opts[tree_dat$node %in% children])) == 0) {

          # if prefer_intersect == T, try set intersect before union
          if (prefer_intersect == T) {
            tree_dat$trait_opts[[i]] <- dplyr::intersect(unlist(tree_dat$trait_opts[tree_dat$node == children[1]]),
                                                         unlist(tree_dat$trait_opts[tree_dat$node == children[2]]))
            # if the intersect is empty, use set union
            if (length(tree_dat$trait_opts[[i]]) == 0) {
              tree_dat$trait_opts[[i]] <- dplyr::union(tree_dat$trait_opts[tree_dat$node == children[1]],
                                                       tree_dat$trait_opts[tree_dat$node == children[2]])
            }
          } else if (prefer_intersect == F) {
            # if prefer_intersect == F, use the set union
            tree_dat$trait_opts[[i]] <- dplyr::union(tree_dat$trait_opts[tree_dat$node == children[1]],
                                                     tree_dat$trait_opts[tree_dat$node == children[2]])
          }
          # union might produce a list with duplicates - remove these
          tree_dat$trait_opts[[i]] <- unique(unlist(tree_dat$trait_opts[[i]]))

        } # ... with two solved child nodes
      } # identify unsolved node ...
    } # for loop running through rows
  } # while loop

  # option to resolve unambiguous nodes
  if (resolve_unambiguous == TRUE) {
    # make new column for inferred result if required
    if (inferred_column %in% names(tree_dat) == F) {
      tree_dat[, inferred_column] <- NA
    }

    # at each node, if there is only one option (length == 1), return it
    for (i in 1:nrow(tree_dat)) {
      if (length(unlist(tree_dat$trait_opts[[i]])) == 1) {
        tree_dat[i, inferred_column] <- tree_dat$trait_opts[[i]][1]
      }
    }
  } # option to resolve unambiguous nodes

  tree_dat

}
